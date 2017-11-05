/// A service that can be used to register async functions.
module IVR.AsyncService

open System
open System.Threading
open System.Diagnostics
open System.Runtime.ExceptionServices

[<NoEquality; NoComparison>]
type internal UnsafeRegistry = {
    Cancellation: CancellationTokenSource
    SyncRoot: obj
    mutable Active: int
    OnException: exn -> unit
}

type JoinResult = 
    | Timeout
    | Completed

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module private UnsafeRegistry = 

    let initial = {
        Cancellation = new CancellationTokenSource()
        SyncRoot = obj()
        Active = 0
        OnException = fun e -> ExceptionDispatchInfo.Capture(e).Throw()
    }

    let register registry = 
        lock registry.SyncRoot
        <| fun () ->
        registry.Active <- registry.Active + 1
        Monitor.PulseAll registry.SyncRoot

    let unregister registry = 
        lock registry.SyncRoot
        <| fun () ->
        assert(registry.Active <> 0)
        registry.Active <- registry.Active - 1
        Monitor.PulseAll registry.SyncRoot

    /// Run an async job in the context of the registry. Note that the
    /// job itself must be deferred, so that the registration covers the
    /// complete dispatching process of a message.
    let run (job: unit -> Async<unit>) registry = 
        register registry

        let registeredJob = async {
            try
                try
                    do! job()
                finally
                    unregister registry
            with e ->
                registry.OnException e
        }

        Async.Start(registeredJob, registry.Cancellation.Token)

    let join (timeout: TimeSpan) registry =
        let ticksPerSecond = float Stopwatch.Frequency
        let timeoutAt = Stopwatch.GetTimestamp() + (int64 (timeout.TotalSeconds * ticksPerSecond))
        // tbd: make this simpler (probably by polling)!
        // this is the standard condition variable wait pattern
        // https://en.wikipedia.org/wiki/Monitor_(synchronization) (Monitor usage)
        lock registry.SyncRoot <| 
        fun () ->
        let rec loop() =
            if registry.Active = 0 then Completed else
            let now = Stopwatch.GetTimestamp()
            if timeoutAt <= now then JoinResult.Timeout else
            let maxWait = timeoutAt - now
            if Monitor.Wait(registry.SyncRoot, TimeSpan.FromSeconds(float maxWait / ticksPerSecond)) 
            then loop()
            else JoinResult.Timeout
        loop()

    /// Wait for all async jobs to be completed within the given timeout. Cancel them
    /// and wait again if they did not complete.
    let joinAndCancel (joinTimeout, joinAfterCancelTimeout) registry = 
        match join joinTimeout registry with
        | JoinResult.Timeout -> 
            registry.Cancellation.Cancel()
            join joinAfterCancelTimeout registry
        | _ -> 
            JoinResult.Completed

[<NoEquality; NoComparison>]
type AsyncServiceBuilder = internal {
    Registry: UnsafeRegistry
    Service: Runtime.IServiceContext -> Flux.Request -> Flux.Response option
}

/// Create a new async service builder.
let createBuilder() = { 
    Registry = UnsafeRegistry.initial
    Service = fun _ _ -> None
}

/// Return the service and a join function that can be used to shut down unsafe async services
/// in a gentle way.
let build builder = 
    builder.Service,
    fun timeouts -> UnsafeRegistry.joinAndCancel timeouts builder.Registry

let private addDispatcher dispatcher (builder: AsyncServiceBuilder) = 

    { builder with
        Service = fun context request ->
            match builder.Service context request with
            | Some response -> Some response
            | None -> dispatcher context request
    }

/// Add an async service function to the builder.
let add (f: 'e -> Async<'response> when 'e :> IVR.IAsyncRequest<'response>) (builder: AsyncServiceBuilder) =
        
    let dispatch (context: Runtime.IServiceContext) : (Flux.Request -> Flux.Response option) =
        function
        | :? 'e as r -> 
            let id = IVR.generateAsyncRequestId()
            let scheduleResponse (r: 'response IVR.result)  = 
                IVR.AsyncResponse(id, r)
                |> context.ScheduleEvent
                
            Async.Start <| async {
                try
                    let! response = f r
                    IVR.Value response
                    |> scheduleResponse
                with e ->
                    IVR.Error e
                    |> scheduleResponse
            }
            Some <| box id
        | _ -> 
            None

    addDispatcher dispatch builder

/// Add an async service function that is considered unsafe, meaning that
/// its result is ignored. This is useful for situations in which
/// an async service function may be used in a finalizer (which can never
/// wait for an event / result), _and_ it's failure is not considered fatal.
let addUnsafe (f: 'e -> Async<unit> when 'e :> IVR.IRequest<unit>) builder =

    let registry = builder.Registry

    let dispatch _ : (Flux.Request -> Flux.Response option) =
        function
        | :? 'e as r -> 
            UnsafeRegistry.run (fun () -> f r) registry
            Some <| box ()
        | _ -> 
            None

    addDispatcher dispatch builder

/// Track exceptions of unsafe functions. Call this function before adding any 
/// unsafe functions that need their exceptions to be tracked.
let trackException f builder = 
    { builder with Registry = { builder.Registry with OnException = f } }

/// A service that can be used to register async functions.
module IVR.AsyncService

open System
open System.Threading
open System.Diagnostics

[<NoEquality; NoComparison>]
type internal UnsafeRegistry = {
    Cancellation: CancellationTokenSource
    SyncRoot: obj
    mutable Active: int
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

    let run (job: Async<unit>) registry = 
        register registry

        let registeredJob = async {
            try
                do! job
            finally
                unregister registry
        }

        Async.Start(registeredJob, registry.Cancellation.Token)

    let join (timeout: TimeSpan) registry =
        let ticksPerSecond = float Stopwatch.Frequency
        let timeoutAt = Stopwatch.GetTimestamp() + (int64 (timeout.TotalSeconds * ticksPerSecond))
        // tbd: make this simpler (probably by polling)!
        // this is the standard condition variable wait pattern
        // https://en.wikipedia.org/wiki/Monitor_(synchronization) (Monitor usage)
        lock registry.SyncRoot 
        <| fun () ->
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
    IdGenerator: Ids.Generator
    Registry: UnsafeRegistry
    Service: Runtime.IServiceContext -> Request -> Response option
}

/// Create a new async service builder.
let createBuilder() = { 
    IdGenerator = Ids.newGenerator()
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
        
    let dispatch (context: Runtime.IServiceContext) : (Request -> Response option) =
        function
        | :? 'e as r -> 
            let id = builder.IdGenerator.GenerateId()
            let scheduleResponse (r: 'response IVR.result)  = 
                IVR.AsyncResponse(id, r)
                |> context.ScheduleEvent
                
            Async.Start <| async {
                try
                    let! response = f r
                    Value response
                    |> scheduleResponse
                with e ->
                    Error e
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

    let dispatch _ : (Request -> Response option) =
        function
        | :? 'e as r -> 
            UnsafeRegistry.run (f r) registry
            Some <| box ()
        | _ -> 
            None

    addDispatcher dispatch builder

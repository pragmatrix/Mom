﻿/// A service that can be used to register async functions.
module Mom.AsyncService

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

[<Struct; RequireQualifiedAccess>]
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
    /// creation of the computation and the computation itself.
    let run (job: unit -> Async<unit>) registry = 
        register registry

        try
            let computation = job()

            let registeredJob = async {
                try
                    try
                        do! computation
                    finally
                        unregister registry
                with e ->
                    registry.OnException e
            }

            Async.Start(registeredJob, registry.Cancellation.Token)
        with e ->
            registry.OnException e

    let join (timeout: TimeSpan) registry =
        let ticksPerSecond = float Stopwatch.Frequency
        let timeoutAt = Stopwatch.GetTimestamp() + (int64 (timeout.TotalSeconds * ticksPerSecond))
        // tbd: make this simpler (probably by polling)!
        // this is the standard condition variable wait pattern
        // https://en.wikipedia.org/wiki/Monitor_(synchronization) (Monitor usage)
        lock registry.SyncRoot <| 
        fun () ->
        let rec loop() =
            if registry.Active = 0 then JoinResult.Completed else
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
let add (f: 'e -> Async<'response> when 'e :> Mom.IAsyncRequest<'response>) (builder: AsyncServiceBuilder) =
        
    let dispatch (context: Runtime.IServiceContext) : (Flux.Request -> Flux.Response option) =
        function
        | :? 'e as r -> 
            let id = Mom.generateAsyncRequestId()
            let scheduleResponse (r: 'response Flux.result)  = 
                Mom.AsyncResponse(id, r)
                |> context.ScheduleEvent

            // Note that we instantiate the computation synchronously! This
            // allows the function to
            // - throw exceptions directly into the same mom calling context.
            // - makes it possible for it synchronize with other asynchronous calls.
            let computation = f r
            Async.Start <| async {
                try
                    let! response = computation
                    Flux.Value response
                    |> scheduleResponse
                with e ->
                    Flux.captureException e
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
let addUnsafe (f: 'e -> Async<unit> when 'e :> Mom.IRequest<unit>) builder =

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

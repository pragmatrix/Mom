[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module IVR.Runtime

open System
open Threading

///
/// The IVR runtime.
/// 

// predefined runtime events

/// This event can be sent to the Runtime to try to cancel the current IVR.
type CancelIVR = CancelIVR

/// The stuff a service can do.
type IServiceContext = 
    abstract ScheduleEvent : Event -> unit
    abstract PostRequest : Request -> unit

/// This is the runtime that drives the IVR.
type Runtime internal (eventQueue: SynchronizedQueue<Event>, host: IServiceContext -> Host) as this = 

    // Partially apply the runtime to the host, so that hosts can initialize.
    let host = host this

    interface IDisposable with
        member this.Dispose() = this.Cancel()

    interface IServiceContext with
        member this.ScheduleEvent event = this.ScheduleEvent event
        member __.PostRequest request = host request |> ignore

    /// Asynchronously schedules an event to the runtime.
    member __.ScheduleEvent (event : Event) = 
        eventQueue.Enqueue event

    member private this.Cancel() = 
        this.ScheduleEvent CancelIVR

    /// Runs the ivr synchronously. Returns Some value or None if the ivr was cancelled.
    member __.Run ivr = 

        let rec runLoop flux =
            match flux with
            | Completed c -> 
                match c with 
                | Value r -> Some r
                | Error e -> raise e
                | Cancelled -> None
            | Requesting (request, cont) -> 
                let result = 
                    try
                        host request 
                        |> Value
                    with e ->
                        e |> Error
                result |> cont |> runLoop
            | Waiting cont ->
                let event = eventQueue.Dequeue()
                match event with
                | :? CancelIVR -> IVR.tryCancel flux
                | event -> cont event
                |> runLoop 

        ivr
        |> IVR.start
        |> runLoop

    member this.Run (ivr, cancellationToken: CancellationToken) = 
        use __ = cancellationToken.Register(fun () -> this.Cancel())
        this.Run ivr

/// Defines a service. 
/// Note that a service's context is partially applied per Runtime. 
/// This way the service can associate its own instance variables with the Runtime.
/// Note that the return value indicates not only the response itself, it also notifies the
/// runtime if a request is handled, so if the request is asynchronous and can not actually
/// return a reponse, () should be returned when the request is considered to be processed.
type Service = IServiceContext -> Request -> Response option
        
/// A builder that supports the creation of runtimes and adding services to it.
[<NoComparison;NoEquality>]
type Builder = {
    EventQueue: SynchronizedQueue<Event>
    Services: Service list
    Closed: bool
}

let builder = { EventQueue = SynchronizedQueue<Event>(); Services = []; Closed = false }

let withEventQueue queue builder = 
    { builder with EventQueue = queue }

let withService (service: Service) builder = 
    { builder with
        Services = service :: builder.Services }

let private flip f a b = f b a
    
let withServices (services: Service list) builder = 
    services |> List.fold (flip withService) builder

let create builder = 

    let services = builder.Services |> List.rev

    let serviceHost runtime =
        // parameterize services with the runtime
        let services = services |> List.map ((|>) runtime)
        fun (cmd: Request) ->
            services
            |> List.tryPick (fun s -> s cmd)
            |> function 
            | None -> failwithf "%A: request unhandled" cmd
            | Some response -> response

    new Runtime (builder.EventQueue, serviceHost)

/// Some predefined services that should be supported by every runtime.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Service = 

    let schedule (context: IServiceContext) (cmd: Request) = 
        match cmd with
        | :? IVR.Schedule as s -> s.Event |> context.ScheduleEvent; () |> box |> Some
        | _ -> None

    let delay (context: IServiceContext) =
        let delayIdGenerator = Ids.newGenerator()
        fun (cmd : Request) ->
            match cmd with
            | :? IVR.Delay as d -> 
                let (IVR.Delay timespan) = d
                let id = delayIdGenerator.GenerateId()
                let callback _ = context.ScheduleEvent (IVR.DelayCompleted id)
                new Timer(callback, null, int64 timespan.TotalMilliseconds, -1L) |> Operators.ignore
                id |> box |> Some
            | _ -> None

    let async (context: IServiceContext) =
        let asyncIdGenerator = Ids.newGenerator()
        fun (cmd: Request) ->
            match cmd with
            | :? IVR.IAsyncComputation as ac -> 
                let id = asyncIdGenerator.GenerateId()
                ac.Run(fun r ->
                    context.ScheduleEvent (IVR.AsyncComputationCompleted(id, r))
                    )
                id |> box |> Some
            | _ -> None

    /// Forward the msg to _all_ the services specified. These services are a treated as message sinks.
    /// Only if none of the services processes the message, the delivrity mechanism continues to services outside of this
    /// forwarding group.
    let forward (services: Service list) =
        fun runtime ->
            let services = services |> List.map ((|>) runtime)
            fun message ->
                // if one or more of the services consume the event, it's returned as consumed
                let consumed = 
                    services 
                    |> List.map (fun service -> 
                        match service message with
                        | Some v when v = box () -> true
                        | Some v -> failwithf "a service that is in a forwarding group, returned an unexpected value: %A" v
                        | None -> false
                        ) 
                    |> List.contains true
                if consumed
                then () |> box |> Some
                else None

    let disabled _ _ = None

    [<NoComparison;NoEquality>]
    type ServiceCrashResponse =
        /// Return the response and continue the service that crashed
        | ContinueService
        /// Replace the service with a new one (use Service.empty to disable the service)
        | ReplaceService of Service
            
    /// Disable the service that crashed
    let DisableService = ReplaceService disabled

    type ServiceCrashResponder = IServiceContext -> Request -> exn -> ServiceCrashResponse

    let protect (responder: ServiceCrashResponder) (service: Service) : Service =
        fun context ->
            let mutable current = service context
            fun request ->
            try
                current request
            with e ->
                let crashResponse = responder context request e
                match crashResponse with
                | ReplaceService service -> current <- service context
                | ContinueService -> ()
                None

/// Creates a default builder, that includes the services schedule, delay, and async.
let defaultBuilder = 
    builder
    |> withService Service.schedule
    |> withService Service.delay
    |> withService Service.async

/// Builds a default runtime that forwards all requests to the host service. The runtime includes the
/// services schedule, delay, and async.
let newRuntime hostService = 
    defaultBuilder
    |> withService hostService
    |> create
    

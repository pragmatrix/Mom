[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Mom.Runtime

open System
open System.Threading
open Mom.Threading

///
/// The Mom runtime.
/// 

// predefined runtime events

/// This event can be sent to the Runtime to try to cancel the current Mom.
[<Struct>]
type CancelMom = 
    | CancelMom

/// The stuff a service can do.
type IServiceContext = 
    abstract ScheduleEvent : Flux.Event -> unit
    abstract PostRequest : Flux.Request -> unit

type Host = Flux.Request -> Flux.Response

/// This is the runtime that drives the Mom.
type Runtime internal (eventQueue: SynchronizedQueue<Flux.Event>, host: IServiceContext -> Host) as this = 

    // Partially apply the runtime to the host, so that hosts can initialize.
    let host = host this

    interface IDisposable with
        member this.Dispose() = this.Cancel()

    interface IServiceContext with
        member this.ScheduleEvent event = this.ScheduleEvent event
        member __.PostRequest request = host request |> ignore

    /// Asynchronously schedules an event to the runtime.
    member __.ScheduleEvent (event : Flux.Event) = 
        eventQueue.Enqueue event

    member private this.Cancel() = 
        this.ScheduleEvent CancelMom

    /// Runs the mom synchronously. Returns `Some(value)` or `None` if the mom was cancelled.
    member __.Run mom = 

        let rec runLoop flux =
            match flux with
            | Flux.Completed c -> 
                match c with 
                | Flux.Value r 
                    -> Some r
                | Flux.Error e 
                    -> e.Throw(); None
                | Flux.Cancelled -> None
            | Flux.Requesting (request, cont) -> 
                let result = 
                    try
                        host request 
                        |> Flux.Value
                    with e ->
                        Flux.captureException e
                result |> cont |> runLoop
            | Flux.Waiting cont ->
                let event = eventQueue.Dequeue()
                match event with
                | :? CancelMom -> Flux.cancel flux
                | event -> cont event
                |> runLoop 

        mom
        |> Mom.start
        |> runLoop

    member this.Run (mom, cancellationToken: CancellationToken) = 
        use __ = cancellationToken.Register(fun () -> this.Cancel())
        this.Run mom

/// Defines a service. 
/// Note that a service's context is partially applied per Runtime. 
/// This way the service can associate its own instance variables with the Runtime.
/// Note that the return value indicates not only the response itself, it also notifies the
/// runtime if a request is handled, so if the request is asynchronous and can not actually
/// return a reponse, () should be returned when the request is considered to be processed.
type Service = IServiceContext -> Flux.Request -> Flux.Response option
        
/// A builder that supports the creation of runtimes and adding services to it.
[<Struct; NoComparison; NoEquality>]
type Builder = {
    EventQueue: SynchronizedQueue<Flux.Event>
    Services: Service list
}

/// Create a new builder instance.
///
/// The `EventQueue` inside the builder mutates, therefore we can not share builder instances
/// between runtimes. Meaning that new builders need to be created for each.
///
/// TODO: Make sure that we can't share Event Queues between different runtimes.
let newBuilder() = { EventQueue = SynchronizedQueue<Flux.Event>(); Services = [] }

let withEventQueue queue builder = 
    { builder with EventQueue = queue }

let withService (service: Service) builder = 
    { builder with
        Services = service :: builder.Services }

let private flip f a b = f b a

/// Adds a number of services.    
let withServices (services: Service list) builder = 
    services |> List.fold (flip withService) builder

/// Adds a function that gets run when the particular request was
/// invoked inside the Mom with the type of the function's argument.
let withFunction (f: 'request -> 'r when 'request :> Mom.IRequest<'r>) =
    let service _ (req: Flux.Request) : Flux.Response option =
        match req with
        | :? 'request as req
            -> Some (box (f req))
        | _ -> None

    withService service

let build (builder: Builder) = 

    let services = builder.Services |> List.rev

    let serviceHost runtime =
        // parameterize services with the runtime
        let services = services |> List.map ((|>) runtime)
        fun (cmd: Flux.Request) ->
            services
            |> List.tryPick (fun s -> s cmd)
            |> function 
            | None -> failwithf "Request of type `%O` unhandled, is the service registered?" (cmd.GetType())
            | Some response -> response

    new Runtime (builder.EventQueue, serviceHost)

[<Obsolete("Use Runtime.build")>]
let create builder = build builder

/// Some predefined services that should be supported by every runtime.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Service = 

    let delay (context: IServiceContext) =
        let delayIdGenerator = Ids.newGenerator()
        let mutable activeTimers = Map.empty
        fun (cmd : Flux.Request) ->
            match cmd with
            | :? Mom.Delay as d -> 
                let (Mom.Delay timespan) = d
                let id = delayIdGenerator.GenerateId()
                let callback _ = context.ScheduleEvent (Mom.DelayCompleted id)
                let t = new Timer(callback, null, int64 timespan.TotalMilliseconds, -1L)
                activeTimers <- Map.add id t activeTimers
                id |> box |> Some
            | :? Mom.CancelDelay as cd ->
                let (Mom.CancelDelay id) = cd
                activeTimers <- Map.remove id activeTimers
                () |> box |> Some
            | _ -> None

    let async (context: IServiceContext) =
        let asyncIdGenerator = Ids.newGenerator()
        fun (cmd: Flux.Request) ->
            match cmd with
            | :? Mom.IAsyncComputation as ac -> 
                let id = asyncIdGenerator.GenerateId()
                ac.Run(fun r -> Mom.AsyncComputationCompleted(id, r) |> context.ScheduleEvent)
                id |> box |> Some
            | _ -> None

    /// Forward the msg to _all_ the services specified. These services are a treated as message sinks.
    /// Only if none of the services processes the message, the delivery mechanism continues to services outside of this
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

    [<Struct; NoComparison; NoEquality>]
    type ServiceCrashResponse =
        /// Return the response and continue the service that crashed
        | ContinueService
        /// Replace the service with a new one (use Service.empty to disable the service)
        | ReplaceService of Service
            
    /// Disable the service that crashed
    let DisableService = ReplaceService disabled

    type ServiceCrashResponder = IServiceContext -> Flux.Request -> exn -> ServiceCrashResponse

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

/// Creates a default builder, that includes the services delay, and async.
let newDefaultBuilder() = 
    newBuilder()
    |> withService Service.delay
    |> withService Service.async

/// Builds a default runtime that forwards all requests to the host service. The runtime includes the
/// services delay, and async.
let newRuntime hostService = 
    newDefaultBuilder()
    |> withService hostService
    |> build
    

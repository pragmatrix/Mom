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
    abstract PostCommand : Command -> unit

/// This is the runtime that drives the IVR.
type Runtime internal (eventQueue: SynchronizedQueue<Event>, host: IServiceContext -> Host) as this = 

    // Partially apply the runtime to the host, so that hosts can initialize.
    let host = host this

    interface IDisposable with
        member this.Dispose() = this.Cancel()

    interface IServiceContext with
        member this.ScheduleEvent event = this.ScheduleEvent event
        member this.PostCommand command = host command |> ignore

    /// Asynchronously schedules an event to the runtime.
    member this.ScheduleEvent (event : Event) = 
        eventQueue.Enqueue event

    member private this.Cancel() = 
        this.ScheduleEvent CancelIVR

    /// Runs the ivr synchronously. Returns Some value or None if the ivr was cancelled.
    member this.Run ivr = 

        let rec runLoop ivr = 
            let event = eventQueue.Dequeue()
            match event with
            | :? CancelIVR -> IVR.tryCancel ivr
            | event -> IVR.step event ivr
            |> next 

        and next ivr =
            match ivr with
            | Completed c -> 
                match c with 
                | Value r -> Some r
                | Error e -> raise e
                | Cancelled -> None
            | Active _ -> runLoop ivr
            | Inactive _ -> failwith "internal error, state transition of an ivr from active -> inactive"

        ivr
        |> IVR.start host
        |> next

    member this.Run (ivr, cancellationToken: CancellationToken) = 
        use c = cancellationToken.Register(fun () -> this.Cancel())
        this.Run ivr

/// Defines a service. 
/// Note that a service's context is partially applied per Runtime. 
/// This way the service can associate its own instance variables with the Runtime.
/// Note that the return value indicates not only the response itself, it also notifies the
/// runtime if a command is handled, so if the command is asynchronous and can not actually
/// return a reponse, () should be returned when the command is considered to be processed.
type Service = IServiceContext -> Command -> Response option
        
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

let create builder = 

    let services = builder.Services |> List.rev

    let serviceHost runtime =
        // parameterize services with the runtime
        let services = services |> List.map ((|>) runtime)
        fun (cmd: Command) ->
            services
            |> List.tryPick (fun s -> s cmd)
            |> function 
            | None -> failwithf "%A: command unhandled" cmd
            | Some response -> response

    new Runtime (builder.EventQueue, serviceHost)

/// Some predefined services that should be supported by every runtime.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Service = 

    let schedule (context: IServiceContext) (cmd: Command) = 
        match cmd with
        | :? IVR.Schedule as s -> s.Event |> context.ScheduleEvent; () |> box |> Some
        | _ -> None

    let delay (context: IServiceContext) =
        let delayIdGenerator = Ids.newGenerator()
        fun (cmd : Command) ->
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
        fun (cmd: Command) ->
            match cmd with
            | :? IVR.IAsyncComputation as ac -> 
                let id = asyncIdGenerator.GenerateId()
                ac.Run(fun r ->
                    context.ScheduleEvent (IVR.AsyncComputationCompleted(id, r))
                    )
                id |> box |> Some
            | _ -> None

    /// Forward the msg to all services and expect each either to consume the msg or to ignore it.
    let forward (services: Service list) =
        fun runtime ->
            let services = services |> List.map ((|>) runtime)
            fun obj ->
                // if one or more of the services consume the event, it's returned as consumed
                let consumed = 
                    services 
                    |> List.map (fun srv -> (srv obj).IsSome) 
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

    type ServiceCrashResponder = IServiceContext -> Command -> exn -> ServiceCrashResponse

    let protect (responder: ServiceCrashResponder) (service: Service) : Service =
        fun context ->
            let mutable current = service context
            fun command ->
            try
                current command
            with e ->
                let crashResponse = responder context command e
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

/// Builds a default runtime that forwards all commands to the host service. The runtime includes the
/// services schedule, delay, and async.
let newRuntime hostService = 
    defaultBuilder
    |> withService hostService
    |> create
    

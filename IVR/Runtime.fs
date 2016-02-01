namespace IVR

open System
open Threading

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Runtime =

    //
    // The IVR runtime.
    // 

    // predefined runtime events

    /// This event can be sent to the Runtime to try to cancel the current IVR.
    type CancelIVR = CancelIVR

    /// This is the runtime that drives the IVR.
    type Runtime internal (eventQueue: SynchronizedQueue<Event>, host: Runtime -> Host) as this = 

        // Partially apply the runtime to the host, so that hosts can initialize attached vars.
        let host = host this

        interface IDisposable with
            member this.Dispose() = this.cancel()

        /// Asynchronously schedules an event to the runtime.
        member this.scheduleEvent (event : Event) = 
            eventQueue.enqueue event

        member private this.cancel() = 
            this.scheduleEvent CancelIVR

        /// Runs the ivr synchronously. Returns Some value or None if the ivr was cancelled.
        member this.run ivr = 

            let rec runLoop ivr = 
                let event = eventQueue.dequeue()
                match event with
                | :? CancelIVR -> IVR.tryCancel host ivr
                | event -> IVR.step host event ivr
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

    //
    // A builder that supports the creation of runtimes and adding services to it.
    //

    /// Defines a service. Note that a service's runtime is partially applied. This way the
    /// service can associate its own instance variables with the Runtime.
    type Service = Runtime -> Command -> Response option
        
    [<NoComparison;NoEquality>]
    type Builder = {
            eventQueue: SynchronizedQueue<Event>
            services: Service list
            closed: bool
        }

    let builder = { eventQueue = SynchronizedQueue<Event>(); services = []; closed = false }

    let withEventQueue queue builder = 
        { builder with eventQueue = queue }

    let withService (service: Service) builder = 
        { builder with
            services = service :: builder.services }

    let private close builder = { builder with closed = true }

    let withHost (host: Host) builder = 
        if builder.closed then
            failwith "can not add a host to a closed builder"

        let service _ command = 
            host command |> Some

        builder 
        |> withService service
        |> close

    let create builder = 

        let services = builder.services |> List.rev

        let serviceHost runtime =
            // parameterize services with the runtime
            let services = services |> List.map ((|>) runtime)
            fun (cmd: Command) ->
                services
                |> List.tryPick (fun s -> s cmd)
                |> function 
                | None -> failwithf "%A: command unhandled" cmd
                | Some response -> response

        new Runtime (builder.eventQueue, serviceHost)

    //
    // Some predefined services that should be supported by every runtime.
    //

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Service = 

        let schedule (runtime: Runtime) (cmd: Command) = 
            match cmd with
            | :? IVR.Schedule as s -> s.event |> runtime.scheduleEvent; () |> box |> Some
            | _ -> None

        let delay (runtime: Runtime) =
            let delayIdGenerator = Ids.newGenerator()
            fun (cmd : Command) ->
                match cmd with
                | :? IVR.Delay as d -> 
                    let (IVR.Delay timespan) = d
                    let id = delayIdGenerator.generateId()
                    let callback _ = runtime.scheduleEvent (IVR.DelayCompleted id)
                    new Timer(callback, null, int64 timespan.TotalMilliseconds, -1L) |> Operators.ignore
                    id |> box |> Some
                | _ -> None

        let async (runtime: Runtime) =
            let asyncIdGenerator = Ids.newGenerator()
            fun (cmd: Command) ->
                match cmd with
                | :? IVR.IAsyncComputation as ac -> 
                    let id = asyncIdGenerator.generateId()
                    ac.run(fun r ->
                        runtime.scheduleEvent (IVR.AsyncComputationCompleted(id, r))
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

        type ServiceCrashResponder = Runtime -> Command -> exn -> ServiceCrashResponse

        let protect (responder: ServiceCrashResponder) (service: Service) : Service =
            fun runtime ->
                let mutable current = service
                fun command ->
                try
                    service runtime command
                with e ->
                    let crashResponse = responder runtime command e
                    match crashResponse with
                    | ReplaceService service -> current <- service
                    | ContinueService -> ()
                    None

    /// Runtimes a default builder, that includes the services schedule, delay, and async.
    let defaultBuilder = 
        builder
        |> withService Service.schedule
        |> withService Service.delay
        |> withService Service.async

    /// Builds a default runtime that forwards all commands to the host. The runtime includes the
    /// services schedule, delay, and async.
    let newRuntime host = 
        defaultBuilder
        |> withHost host
        |> create
    

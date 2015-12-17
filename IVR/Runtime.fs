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

    type private CancelIVR = CancelIVR

    type Runtime (host: Runtime -> Host) as this = 

        let host = host this

        let eventQueue = SynchronizedQueue<Event>();

        interface IDisposable with
            member this.Dispose() = this.cancel()

        /// Asynchronously schedules an event to the runtime.
        member this.scheduleEvent (event : Event) = 
            eventQueue.enqueue event

        member private this.cancel() = 
            this.scheduleEvent CancelIVR

        member this.run ivr = 

            let rec runLoop ivr = 
                let event = eventQueue.dequeue()
                match event with
                | :? CancelIVR -> IVR.tryCancel host ivr
                | event -> IVR.step host event ivr
                |> next 

            and next ivr =
                match ivr with
                | Completed (Value r) -> Some r
                | Completed (Error e) -> raise e
                | Active _ -> runLoop ivr
                | Inactive _ -> failwith "internal error, state transition of an ivr from active -> inactive"

            ivr
            |> IVR.start host
            |> next

    //
    // A builder that supports the creation of runtimes and adding services to it.
    //

    type Service = Runtime -> Command -> Response option
        
    [<NoComparison;NoEquality>]
    type Builder = {
            services: Service list
            closed: bool
        }

    let builder = { services = []; closed = false }

    let withService (service: Service) builder = 
        { builder with
            services = service :: builder.services }

    let withHost (host: Host) builder = 
        if builder.closed then
            failwith "can not add a host to a closed builder"

        let service _ command = 
            host command |> Some

        builder |> withService service

    let create builder = 

        let rec serviceHost (services: Service list) runtime command = 
            match services with
            | [] -> () |> box
            | s::todo ->
            match s runtime command with
            | Some response -> response
            | None -> serviceHost todo runtime command
                    
        let services = builder.services |> List.rev
        new Runtime (serviceHost services)

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
    

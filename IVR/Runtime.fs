namespace IVR

open System
open Threading

open IVR

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Runtime =

    //
    // The IVR runtime.
    // 

    // predefined runtime events

    type private CancelIVR = CancelIVR

    type Runtime(host: Host) = 

        let eventQueue = SynchronizedQueue<Event>();

        let delayIdGenerator = Ids.newGenerator()
        let asyncIdGenerator = Ids.newGenerator()

        interface IDisposable with
            member this.Dispose() = this.cancel()

        /// Asynchronously schedules an event to the runtime.
        member this.scheduleEvent (event : Event) = 
            eventQueue.enqueue event

        member private this.cancel() = 
            this.scheduleEvent CancelIVR

        member this.run ivr = 

            let host = this.executeCommand

            let rec runLoop ivr = 
                let event = eventQueue.dequeue()
                match event with
                | :? CancelIVR -> tryCancel host ivr
                | event -> step host event ivr
                |> next 

            and next ivr =
                match ivr with
                | Completed (Result r) -> Some r
                | Completed (Error e) -> raise e
                | Active _ -> runLoop ivr
                | Inactive _ -> failwith "internal error, state transition of an ivr from active -> inactive"

            ivr
            |> start host
            |> next

        member this.executeCommand cmd = 
            match cmd with
            | :? Delay as d -> this.delay d
            | :? IAsyncComputation as ac -> this.asyncComputation ac
            | _ -> cmd |> host

        member this.delay(Delay timespan) = 
            let id = delayIdGenerator.generateId()
            let callback _ = this.scheduleEvent (IVR.DelayCompleted id)
            new Timer(callback, null, int64 timespan.TotalMilliseconds, -1L) |> Operators.ignore
            id |> box
            
        member this.asyncComputation(ac: IAsyncComputation) = 
            let id = asyncIdGenerator.generateId()
            ac.run(fun r ->
                this.scheduleEvent (IVR.AsyncComputationCompleted(id, r))
                )
            id |> box

    let newRuntime host = new Runtime(host)

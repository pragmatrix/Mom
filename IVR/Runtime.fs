namespace IVR

open System
open Threading

open IVR

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Runtime =

    //
    // The IVR runtime.
    // 

    // predefined runtime events

    type private CancelIVR = CancelIVR

    type Runtime(host: Host) = 

        let eventQueue = SynchronizedQueue<Event>();

        let delayIdGenerator = Ids.newGenerator()

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
                | :? CancelIVR -> 
                    cancel host ivr
                    raise Cancelled
                | event ->
                let ivr' = step host event ivr
                next ivr'

            and next ivr =
                match ivr with
                | Completed (Result r) -> Some r
                | Completed (Error e) -> raise e
                | Active _ -> runLoop ivr

            ivr
            |> start host
            |> next

        member this.executeCommand cmd = 
            match cmd with
            | :? Delay as d -> this.delay d
            | _ -> cmd |> host

        member this.delay (Delay timespan) = 
            let id = delayIdGenerator.generateId()
            let callback _ = this.scheduleEvent (IVR.DelayCompleted id)
            new Timer(callback, null, int64 timespan.TotalMilliseconds, -1L) |> Operators.ignore
            id |> box
            
    let newRuntime host = new Runtime(host)

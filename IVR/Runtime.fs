namespace IVR

open System
open Threading

open IVR

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Runtime =

    //
    // The ivr runtime.
    // 

    // predefined runtime events

    type private CancelIVR = CancelIVR

    type Runtime(host: Host) = 

        let eventQueue = SynchronizedQueue<Event>();

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
            | :? SystemCommand as sc ->
                sc |> this.processSystemCommand
            | _ ->
                cmd |> host

        member this.processSystemCommand (sc : SystemCommand) = 
            match sc with
            | Delay (id, timespan) ->
                let callback _ = this.scheduleEvent (IVR.DelayCompleted id)
                new Timer(callback, null, int64 timespan.TotalMilliseconds, -1L) |> Operators.ignore
                null
            
    let newRuntime host = new Runtime(host)

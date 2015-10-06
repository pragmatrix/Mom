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

    type Runtime() = 

        let eventQueue = SynchronizedQueue<Event>();

        interface IDisposable with
            member this.Dispose() = this.cancel()

        member this.dispatch event = 
            eventQueue.enqueue event

        member private this.cancel() = 
            this.dispatch CancelIVR

        member this.processSystemCommand (sc : SystemCommand) = 
            match sc with
            | Delay (id, timespan) ->
                let callback _ = this.dispatch (IVR.DelayCompleted id)
                new Timer(callback, null, int64 timespan.TotalMilliseconds, -1L)

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
            obj()
            
    let newRuntime() = new Runtime()

namespace IVR

open System
open Threading

open IVR

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Host =

    //
    // The ivr host, where all events are being dispatched
    // 

    // predefined host events

    type private CancelIVR = CancelIVR

    type Host() = 

        let queue = SynchronizedQueue<obj>();

        interface IDisposable with
            member this.Dispose() = this.cancel()

        member this.dispatch event = 
            queue.enqueue event

        member private this.cancel() = 
            this.dispatch CancelIVR

        member this.serviceCommand (sc : ServiceCommand) = 
            match sc with
            | Delay (id, timespan) ->
                let callback _ = this.dispatch (IVR.DelayCompleted id)
                new Timer(callback, null, int64 timespan.TotalMilliseconds, -1L)

        member this.run ivr = 

            let host = this.executeCommand

            let rec runLoop ivr = 
                let event = queue.dequeue()
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
            
    let newHost() = new Host()

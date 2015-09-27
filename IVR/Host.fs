namespace IVR

open System
open Threading

open IVR

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Host =

    //
    // The ivr host, where all events are being dispatched
    // 

    type Id = int64
    let id = ref 0L
    let newId() = Interlocked.Increment(id)

    // predefined host events

    type private Timeout = Timeout of Id
    type private CancelIVR = CancelIVR

    type Host() = 

        let queue = SynchronizedQueue<obj>();

        interface IDisposable with
            member this.Dispose() = this.cancel()

        member this.dispatch event = 
            queue.enqueue event

        member private this.cancel() = 
            this.dispatch CancelIVR

        member this.delay (timespan : TimeSpan) = 
            ivr {
                let id = newId()
                let callback _ = this.dispatch (Timeout id)
                use _ = new Timer(callback, null, int64 timespan.TotalMilliseconds, -1L)
                do! waitFor' (fun (Timeout tid) -> tid = id)
            }

        member this.run ivr = 

            let host = this.dispatch

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
            
    let newHost() = new Host()

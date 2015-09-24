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

    type Timeout = Timeout of Id
    type CancelIVR = CancelIVR

    type Host = { queue: SynchronizedQueue<obj> }

        with 
        member this.dispatch event = 
            this.queue.enqueue event

        member this.cancel() = 
            this.dispatch CancelIVR

        member this.delay (timespan : TimeSpan) = 
            ivr {
                let id = newId()
                let callback _ = this.dispatch (Timeout id)
                let timer = new Timer(callback, null, int64 timespan.TotalMilliseconds, -1L)
                do! waitFor' (fun (Timeout tid) -> tid = id)
                timer.Dispose()
            }

        member this.run ivr = 
            let rec runLoop ivr = 
                let event = this.queue.dequeue()
                match event with
                | :? CancelIVR -> None
                | event ->
                let ivr' = step ivr event
                match ivr' with
                | Completed r -> Some r
                | Active _ -> runLoop ivr'
                | Delay _ -> failwithf "IVR.run: unexpected: %A" ivr'

            runLoop (start ivr)
    
    let newHost() = { queue = SynchronizedQueue() }

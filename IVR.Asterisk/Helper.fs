namespace IVR.Asterisk

open System.Threading
open System.Collections.Generic

module Helper = 

    // ConcurrentQueue does not quite support what we want here.
    
    type SynchronizedQueue<'t>() = 
        let monitor = obj()
        let queue = Queue<'t>()
        
        member this.enqueue v = 
            lock monitor (
                fun () -> 
                    queue.Enqueue v
                    if queue.Count = 1 then
                        Monitor.Pulse monitor
                )

        member this.dequeue() =
            lock monitor (
                fun () ->
                    while queue.Count = 0 do
                        Monitor.Wait monitor |> ignore

                    queue.Dequeue()
                )

namespace IVR.Tests

open IVR
open NUnit.Framework
open System

[<AutoOpen>]
module Helper = 
    let ivr<'r> = IVR.IVR.ivr<'r>

[<TestFixture>]
type IVRGCTests() =

    [<Test; Category("LongRunning")>]
    member this.longSequentialLoopDoesNotEatUpStackOrMemory() =
    
        // interestingly, the "Generate Tail Calls" option does 
        // not have an effect

        let rec endlessLoop() = ivr {
            do! IVR.wait' (fun _ -> true)
            return! endlessLoop()
            }

        let host = fun _ -> null

        let ivr = IVR.start host (endlessLoop())

        let count = 10000000
        let memTraces = count / 10000
        let memTrace = count / memTraces

        // preallocate a buffer large enough to hold all memory

        let array = Array.zeroCreate memTraces

        let rec stepLoop ivr cnt = 
            if cnt = count then () else
            if (cnt % memTrace = 0) then
                GC.Collect()
                let totalMem = GC.GetTotalMemory(true)
                array.[cnt / memTrace] <- totalMem
            stepLoop (IVR.step null ivr) (cnt+1)

        stepLoop ivr 0

        let memAtStart = array.[0]
        let memAtEnd = array.[memTraces-1]
        let maxMemoryUse = 1000L
        let memoryUse = memAtEnd - memAtStart

        // usually this is about 344 bytes on my machine
        System.Console.WriteLine(sprintf "memory use: %i bytes" memoryUse)
        Assert.That(memoryUse, Is.LessThan(maxMemoryUse))
        
        array
        |> Array.iter (fun mem -> System.Console.WriteLine(sprintf "mem: %i" mem))
     
            

module Mom.Tests.MomGCTests

open System
open FsUnit
open Xunit
open Mom

[<AutoOpen>]
module Helper = 
    let mom<'r> = Mom.Mom.mom<'r>
    let step ev mom = Flux.dispatch ev mom

[<Fact(Skip="brittle")>]
let longSequentialLoopDoesNotEatUpStackOrMemory() =
    
    // interestingly, the "Generate Tail Calls" option does 
    // not have an effect

    let rec endlessLoop() = mom {
        do! Mom.wait' (fun _ -> true)
        return! endlessLoop()
        }

    let mom = Mom.start (endlessLoop())

    let count = 10000000
    let memTraces = count / 10000
    let memTrace = count / memTraces

    // preallocate a buffer large enough to hold all memory

    let array = Array.zeroCreate memTraces

    let rec stepLoop mom cnt = 
        if cnt = count then () else
        if (cnt % memTrace = 0) then
            GC.Collect()
            let totalMem = GC.GetTotalMemory(true)
            array.[cnt / memTrace] <- totalMem
        stepLoop (step null mom) (cnt+1)

    stepLoop mom 0

    let memAtStart = array.[0]
    let memAtEnd = array.[memTraces-1]
    let maxMemoryUse = 1000L
    let memoryUse = memAtEnd - memAtStart

    // usually this is about 344 bytes on my machine
    System.Console.WriteLine(sprintf "memory use: %i bytes" memoryUse)
    memoryUse |> should lessThan maxMemoryUse
        
    array
    |> Array.iter (fun mem -> System.Console.WriteLine(sprintf "mem: %i" mem))
     
            

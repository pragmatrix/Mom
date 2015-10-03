namespace IVR.Tests

open IVR

open NUnit.Framework
open FsUnit
open System.Collections.Generic

type TraceEvent1 = TraceEvent1 of int

[<TestFixture>]
type TracingTests() = 


    [<Test>]
    member this.simpleTraceAndReplay() =

        let test = ivr {
            let! x = IVR.waitFor (fun (TraceEvent1 x) -> Some x)
            yield x
            return x
        }

        let sessionInfo = Tracing.sessionInfo "" 0L null
        let trace = List<string>()
        let tracer = Tracers.stringTracer sessionInfo (fun _ str -> trace.Add(str))
        let test = 
            test 
            |> Tracing.trace tracer
        let host = fun _ -> ()

        test
        |> IVR.start host
        |> IVR.step host (TraceEvent1 10)
        |> ignore

        printf "%A" (trace.ToArray())

        ()


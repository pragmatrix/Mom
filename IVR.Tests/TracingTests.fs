namespace IVR.Tests

open IVR

open NUnit.Framework
open Tracing

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
        let mutable trace = None
        let tracer = Tracers.memoryTracer sessionInfo (fun t -> trace <- Some t)
        let test = 
            test 
            |> Tracing.trace tracer
        let host = fun _ -> ()

        test
        |> IVR.start host
        |> IVR.step host (TraceEvent1 10)
        |> ignore

        trace.Value
        |> Format.trace
        |> Seq.iter (fun s -> printfn "%s" s)

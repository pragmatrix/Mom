namespace IVR.Tests

open IVR

open NUnit.Framework
open Tracing

type TraceEvent1 = TraceEvent1 of int

[<TestFixture>]
type TracingTests() = 

    [<Test>]
    member this.simpleTraceAndReplay() =

        let ivr = ivr {
            let! x = IVR.waitFor (fun (TraceEvent1 x) -> Some x)
            yield x
            return x
        }


        let sessionInfo = Tracing.sessionInfo "" 0L null
        let mutable trace = None
        let tracer = Tracers.memoryTracer sessionInfo (fun t -> trace <- Some t)
        let traced = 
            ivr 
            |> Tracing.trace tracer
        let host = fun _ -> ()

        traced
        |> IVR.start host
        |> IVR.step host (TraceEvent1 10)
        |> ignore

        let trace = trace.Value
        trace
        |> Format.trace
        |> Seq.iter (fun s -> printfn "%s" s)

        trace
        |> Tracing.replay (fun _ -> ivr)
        |> Seq.iter (fun d -> printfn "%A" d)


    [<Test>]
    member this.simpleTraceBinarySerializationTest() = 

        let ivr = ivr {
            let! x = IVR.waitFor (fun (TraceEvent1 x) -> Some x)
            yield x
            return x
        }

        let sessionInfo = Tracing.sessionInfo "" 0L null
        let fn = "test.trace"
        let tracer = Tracers.fileTracer fn sessionInfo
        let traced = 
            ivr 
            |> Tracing.trace tracer
        let host = fun _ -> ()

        traced
        |> IVR.start host
        |> IVR.step host (TraceEvent1 10)
        |> ignore

        let trace = Tracers.readFileTrace fn
        
        trace
        |> Tracing.replay (fun _ -> ivr)
        |> Seq.iter (fun d -> printfn "%A" d)


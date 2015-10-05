﻿namespace IVR.Tests

open IVR
open Tracing

open NUnit.Framework
open FsUnit

type TraceEvent1 = TraceEvent1 of int

type ConvertIntToStr = ConvertIntToStr of int
    with
    interface IVR.IReturns<string>

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
        let host = fun _ -> null

        traced
        |> IVR.start host
        |> IVR.step host (TraceEvent1 10)
        |> ignore

        let trace = trace.Value
        trace
        |> Format.trace
        |> Seq.iter (printfn "%s")

        trace
        |> Tracing.replay (fun _ -> ivr)
        |> fun r -> r.steps
        |> Seq.iter (printfn "%A")


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
        let host = fun _ -> null

        traced
        |> IVR.start host
        |> IVR.step host (TraceEvent1 10)
        |> ignore

        let trace = Tracers.readFileTrace fn
        
        trace
        |> Tracing.replay (fun _ -> ivr)
        |> (fun r -> r.steps)
        |> Seq.iter (printfn "%A")

    [<Test>]
    member this.replayReplaysCommandResponses() = 
        let host (cmd: obj) : obj =
            match cmd with
            | :? ConvertIntToStr as ci ->
                let (ConvertIntToStr i) = ci
                i.ToString() |> box
            | _ -> failwith "internal error"

        let ivr = ivr {
            return! IVR.send (ConvertIntToStr 10)
        }

        // tbd: prettify tracing APIs for common cases and tests! This is ugly as ....!

        let sessionInfo = Tracing.sessionInfo "" 0L null
        let mutable trace = None
        let tracer = Tracers.memoryTracer sessionInfo (fun t -> trace <- Some t)
        
        let traced = ivr |> Tracing.trace tracer

        IVR.start host traced
        |> IVR.result
        |> should equal ("10" |> IVR.Result)

        trace.Value
        |> Format.trace
        |> Seq.iter (printfn "%s")

        let report = 
            trace.Value
            |> Tracing.replay (fun _ -> ivr)
        
        report.incidents |> Seq.iter (printfn "%A")
        report.isEmpty |> should equal true

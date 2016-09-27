module IVR.Tests.TracingTests

open FsUnit
open Xunit
open IVR
open Tracing

type TraceEvent1 = TraceEvent1 of int

type ConvertIntToStr = 
    | ConvertIntToStr of int
    interface IVR.ICommand<string>


[<Fact>]
let simpleTraceAndReplay() =

    let ivr = ivr {
        let! x = IVR.waitFor (fun (TraceEvent1 x) -> Some x)
        do! IVR.post x
        return x
    }


    let sessionInfo = Tracing.sessionInfo "" (Id 0L) null
    let mutable trace = None
    let tracer = Tracers.memoryTracer sessionInfo (fun t -> trace <- Some t)

    let traced = 
        ivr 
        |> Tracing.trace tracer
    let host = fun _ -> null

    traced
    |> IVR.start host
    |> IVR.step (TraceEvent1 10)
    |> ignore

    let trace = trace.Value
    trace
    |> Format.trace
    |> Seq.iter (printfn "%s")

    trace
    |> Tracing.replay (fun _ -> ivr)
    |> fun r -> r.Steps
    |> Seq.iter (printfn "%A")


[<Fact>]
let simpleTraceBinarySerializationTest() = 

    let ivr = ivr {
        let! x = IVR.waitFor (fun (TraceEvent1 x) -> Some x)
        do! IVR.post x
        return x
    }

    let sessionInfo = Tracing.sessionInfo "" (Id 0L) null
    let fn = "test.trace"
    let tracer = Tracers.fileTracer fn sessionInfo
    let traced = 
        ivr 
        |> Tracing.trace tracer
    let host = fun _ -> null

    traced
    |> IVR.start host
    |> IVR.step (TraceEvent1 10)
    |> ignore

    let trace = Tracers.readFileTrace fn
        
    trace
    |> Tracing.replay (fun _ -> ivr)
    |> (fun r -> r.Steps)
    |> Seq.iter (printfn "%A")

[<Fact>]
let replayReplaysCommandResponses() = 
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

    let sessionInfo = Tracing.sessionInfo "" (Id 0L) null
    let mutable trace = None
    let tracer = Tracers.memoryTracer sessionInfo (fun t -> trace <- Some t)
        
    let traced = ivr |> Tracing.trace tracer

    IVR.start host traced
    |> IVR.result
    |> should equal ("10" |> IVR.Value)

    trace.Value
    |> Format.trace
    |> Seq.iter (printfn "%s")

    let report = 
        trace.Value
        |> Tracing.replay (fun _ -> ivr)
        
    report.Incidents |> Seq.iter (printfn "%A")
    report.IsEmpty |> should equal true


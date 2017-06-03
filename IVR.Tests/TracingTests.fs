module IVR.Tests.TracingTests

open FsUnit
open Xunit
open IVR
open Tracing

type TraceEvent1 = TraceEvent1 of int

type TraceRequest = 
    | TraceRequest
    interface IVR.IRequest<unit>

type ConvertIntToStr = 
    | ConvertIntToStr of int
    interface IVR.IRequest<string>

// tbd: IVR API candidate!
let respond r =
    function 
    | Flux.Requesting (_, cont) -> cont (r |> box |> IVR.Value)
    | _ -> failwith "internal error"

/// IVR under test.
let ivr p = ivr {
    let! x = IVR.waitFor (fun (TraceEvent1 x) -> Some x)
    do! IVR.send TraceRequest
    return x * p
}

[<Fact>]
let createTrace() =

    let traced = ivr |> Tracing.trace 5

    let trace = 
        traced
        |> IVR.start
        |> Flux.dispatch (TraceEvent1 10)
        |> respond ()
        |> Flux.resultValue

    trace

[<Fact>]
let simpleTraceAndReplay() =

    let trace = createTrace()

    trace
    |> Format.trace
    |> Seq.iter (printfn "%s")

    trace
    |> Tracing.replay ivr
    |> printfn "%A"

[<Fact>]
let simpleTraceBinarySerializationTest() = 

    let trace = createTrace()

    let t2 : Trace<int, int> = 
        trace
        |> Trace.serialize
        |> Trace.deserialize

    t2 |> should equal trace

#if false

// tbd: Create a more complex tracing / replaying example.

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

#endif

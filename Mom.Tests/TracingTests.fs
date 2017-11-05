module Mom.Tests.TracingTests

open FsUnit
open Xunit
open Mom
open Tracing

type TraceEvent1 = TraceEvent1 of int

type TraceRequest = 
    | TraceRequest
    interface Mom.IRequest<unit>

type ConvertIntToStr = 
    | ConvertIntToStr of int
    interface Mom.IRequest<string>

// tbd: Mom API candidate!
let respond r =
    function 
    | Flux.Requesting (_, cont) -> cont (r |> box |> Flux.Value)
    | _ -> failwith "internal error"

/// Mom under test.
let mom p = mom {
    let! x = Mom.waitFor (fun (TraceEvent1 x) -> Some x)
    do! Mom.send TraceRequest
    return x * p
}

[<Fact>]
let createTrace() =

    let traced = mom |> Tracing.trace 5

    let trace = 
        traced
        |> Mom.start
        |> Flux.dispatch (TraceEvent1 10)
        |> respond ()
        |> Flux.value

    trace

[<Fact>]
let simpleTraceAndReplay() =

    let trace = createTrace()

    trace
    |> Format.trace
    |> Seq.iter (printfn "%s")

    trace
    |> Tracing.replay mom
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

    let mom = mom {
        return! Mom.send (ConvertIntToStr 10)
    }

    // tbd: prettify tracing APIs for common cases and tests! This is ugly as ....!

    let sessionInfo = Tracing.sessionInfo "" (Id 0L) null
    let mutable trace = None
    let tracer = Tracers.memoryTracer sessionInfo (fun t -> trace <- Some t)
        
    let traced = mom |> Tracing.trace tracer

    Mom.start host traced
    |> Mom.result
    |> should equal ("10" |> Mom.Value)

    trace.Value
    |> Format.trace
    |> Seq.iter (printfn "%s")

    let report = 
        trace.Value
        |> Tracing.replay (fun _ -> mom)
        
    report.Incidents |> Seq.iter (printfn "%A")
    report.IsEmpty |> should equal true

#endif

module IVR.Traces

open System
open System.IO
open System.Diagnostics
open MBrace.FsPickler
open Tracing

module Helper = 

    let private invalidFilenameChars = Path.GetInvalidFileNameChars()

    let toFilename (str : string) = 
        str.ToCharArray() 
        |> Array.map (fun c -> if Array.IndexOf(invalidFilenameChars, c) = -1 then c else '_') 
        |> String

    let serializer = FsPickler.CreateBinarySerializer()

/// Traces entries by receiving a session info and subsequent StepTraces and then attaches timing information to it.
/// Resulting into a TraceHeader and a number of TraceSteps.

let entryTracer (sessionInfo : SessionInfo) (receiver: TraceHeader -> TraceStep -> unit) = 
        
    let startTimeAbsolute = DateTimeOffset.UtcNow
    // use the stopwatch to avoid time skews.
    let stopWatch = Stopwatch.StartNew()
    let header = startTimeAbsolute, sessionInfo
    let stepReceiver = receiver header

    fun (step: StepTrace) ->
        let stepF = stopWatch.Elapsed, step
        stepReceiver stepF

/// Receives traces, collects them all, and finishes up by writing them to the trace receiver.
let traceReceiver (traceReceiver: Trace -> unit) = 
    fun (header: TraceHeader) ->
        let mutable steps = []
        fun (step: TraceStep) ->
            steps <- step :: steps
            if (snd step).hasResult then
                (header, (steps |> List.rev)) |> traceReceiver

/// Receives traces and writes them to a stream in binary format.
let binaryStreamReceiver (stream: Stream) =
    fun (header: TraceHeader) ->
        Helper.serializer.Serialize(stream, header, leaveOpen = true)
        fun (step: TraceStep) ->
            let lastStep = (snd step).hasResult
            Helper.serializer.Serialize(stream, step, leaveOpen = not lastStep)
                
let streamTracer sessionInfo stream =
    stream
    |> binaryStreamReceiver
    |> entryTracer sessionInfo

let filename (sessionInfo: SessionInfo) : string =
    sprintf "%s.%d.trace.bin" (sessionInfo.name |> sprintf "%A" |> Helper.toFilename) sessionInfo.id
        
let fileTracer fn (sessionInfo : SessionInfo) =
    let f = File.Open(fn, FileMode.Create, FileAccess.Write, FileShare.None)
    f |> streamTracer sessionInfo

let readFileTrace (fn: string) : Trace = 
    use stream = File.Open(fn, FileMode.Open, FileAccess.Read, FileShare.Read)
    let (header : TraceHeader) = Helper.serializer.Deserialize(stream, leaveOpen = true)

    let rec readSteps steps = 
        let (step : TraceStep) = Helper.serializer.Deserialize(stream, leaveOpen = true)
        let steps = step :: steps
        if (snd step).hasResult then steps
        else readSteps steps

    let steps = 
        readSteps []
        |> List.rev

    header, steps

let memoryTracer sessionInfo (receiver: Trace -> unit) =
    traceReceiver receiver
    |> entryTracer sessionInfo
    
namespace IVR

open Tracing
open System
open System.IO
open Nessos.FsPickler
open System.Diagnostics

module Tracers = 

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
        
        let startTimeAbsolute = DateTime.Now
        // use the stopwatch to avoid time skews.
        let stopWatch = Stopwatch.StartNew()
        let header = startTimeAbsolute, sessionInfo
        let stepReceiver = receiver header

        fun (step: StepTrace) ->
            let stepF = stopWatch.Elapsed, step
            stepReceiver stepF

    /// Receives traces, collects them all, and finishes up by writing them to the trace reference.
    let traceReceiver (traceReceiver: Tracing.Trace -> unit) = 
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

    let fileTracer (sessionInfo : SessionInfo) =
        let fn = sprintf "%s.%d.trace.bin" (sessionInfo.name |> sprintf "%A" |> Helper.toFilename) sessionInfo.id
        let f = File.Open(fn, FileMode.Create, FileAccess.Write, FileShare.None)
        f |> streamTracer sessionInfo

    let memoryTracer sessionInfo (receiver: Tracing.Trace -> unit) =
        traceReceiver receiver
        |> entryTracer sessionInfo
    
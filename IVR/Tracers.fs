namespace IVR

open Tracing
open System
open System.IO
open Nessos.FsPickler.Json
open System.Diagnostics

module Tracers = 

    module Helper = 

        let private invalidFilenameChars = Path.GetInvalidFileNameChars()

        let toFilename (str : string) = 
            str.ToCharArray() 
            |> Array.map (fun c -> if Array.IndexOf(invalidFilenameChars, c) = -1 then c else '_') 
            |> String

        let serializer = FsPickler.CreateJsonSerializer(indent = true)

    /// Traces entries.
    let entryTracer (sessionInfo : SessionInfo) (receiver: Format.HeaderEntry -> Format.StepEntry -> unit) = 
        
        let startTimeAbsolute = DateTime.Now
        // use the stopwatch to avoid time skews.
        let stopWatch = Stopwatch.StartNew()
        let header = Format.header startTimeAbsolute sessionInfo
        let stepReceiver = receiver header

        fun (step: StepTrace) ->
            let stepF = Format.step stopWatch.Elapsed step
            stepReceiver stepF

    /// Receives traces and writes them to a string receiver.
    let stringReceiver (receiver: string -> bool -> string -> unit) =
        fun (header: Format.HeaderEntry) ->
            let headerStr = Helper.serializer.PickleToString(header)
            let stepReceiver = receiver headerStr
            fun (step: Format.StepEntry) ->
                Helper.serializer.PickleToString(step)
                |> stepReceiver step.result.IsSome

    /// Receives traces and writes them to a text writer.
    let textWriterReceiver (writer: TextWriter) =
        fun (header: Format.HeaderEntry) ->
            Helper.serializer.Serialize(writer, header, leaveOpen = true)
            fun (step: Format.StepEntry) ->
                Helper.serializer.Serialize(writer, step, leaveOpen = true)
                match step.result with
                | None -> ()
                | Some _ ->
                writer.Close() // this also closes the underlying stream!

    /// Traces in a list of strings (json encoded). The receiver receives each string, the header, and all steps. When the
    /// last line is written, which contains the result, the boolean is set to true, otherwise, it's false.
    let stringTracer (sessionInfo: SessionInfo) (receiver: bool -> string -> unit) = 
        fun header ->
            receiver false header
            fun hasResult step ->
                receiver hasResult step
        |> stringReceiver
        |> entryTracer sessionInfo

    let fileTracer (sessionInfo : SessionInfo) =
        let fn = sprintf "%s.%d.trace.json" (sessionInfo.name |> sprintf "%A" |> Helper.toFilename) sessionInfo.id
        let f = File.Open(fn, FileMode.Create, FileAccess.Write, FileShare.Read)
        let writer = new StreamWriter(f, Text.Encoding.UTF8)

        writer
        |> textWriterReceiver
        |> entryTracer sessionInfo


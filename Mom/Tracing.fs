module Mom.Tracing

open System
open System.IO
open System.Globalization
open System.Diagnostics
open MBrace.FsPickler

open Mom.GlobalExports

//
// Mom Tracing support.
//

type internal result<'r> = Flux.result<'r>
type internal flux<'r> = Flux.flux<'r>

/// A step trace represents a trace for a single step of an Mom. 
[<NoComparison>]
type StepTrace = 
    | EventTrace of Flux.Event
    | RequestTrace of Flux.Request * obj result

[<NoComparison>]
type Trace<'param, 'r> = 
    | Trace of (DateTimeOffset * 'param) * (TimeSpan * StepTrace) list * 'r result

/// Wraps a parameter block and an Mom so that it generates a trace.
let trace (p: 'param) (f : 'param -> 'r mom) : Trace<'param, 'r> mom = 

    fun () ->

        let startTime = DateTimeOffset.UtcNow
        let stopwatch = Stopwatch.StartNew()
        
        let rec next traces flux =
            match flux with
            | Flux.Requesting (request, cont) ->
                Flux.Requesting (request, 
                    fun response -> 
                        response 
                        |> cont
                        |> next ((stopwatch.Elapsed, RequestTrace (request, response)) :: traces))

            | Flux.Waiting (cont) ->
                Flux.Waiting (
                    fun event ->
                    event 
                    |> cont 
                    |> next ((stopwatch.Elapsed, EventTrace event) :: traces))

            | Flux.Completed r ->
                Trace((startTime, p), traces |> List.rev, r)
                |> Flux.Value
                |> Flux.Completed
        
        f p |> Mom.start |> next []

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ReplayReport<'r> = 
    /// StepTrace can not applied to the current flux, or the list of step traces are empty.
    | Diverged of pending: StepTrace list * current: 'r flux
    | ResultDiffers of expected: 'r result * actual: 'r result
    | Completed

/// Replay a trace to an Mom and return a report.
let replay (f: 'param -> 'r mom) (Trace((_, param), traces, expectedResult)) : ReplayReport<'r> = 
    
    let rec next traces flux = 
        match flux with
        | Flux.Requesting (request, cont) ->
            match traces with
            | RequestTrace (r, response) :: traces when r = request ->
                response |> cont |> next traces
            | traces -> ReplayReport.Diverged(traces, flux)
        | Flux.Waiting (cont) ->
            match traces with
            | EventTrace e :: traces ->
                e |> cont |> next traces
            | traces -> ReplayReport.Diverged(traces, flux)
        | Flux.Completed r ->
            match traces with
            | [] when r = expectedResult ->
                ReplayReport.Completed
            | [] ->
                ReplayReport.ResultDiffers(expectedResult, r)
            | traces ->
                ReplayReport.Diverged(traces, flux) 

    f param 
    |> Mom.start 
    |> next (traces |> List.map snd)
        
/// Module to convert traces into a human comprehensible format.
module Format =

    open System.Text.RegularExpressions

    let private dateTime (dt: DateTimeOffset) =
        // Inspired by ISO 8601, 
        // but changed T->_, removed date und time separators, and 3 fractional seconds for presenting milliseconds.
        let localTime = dt.ToLocalTime()
        localTime.ToString("yyyyMMdd_HHmmss.fff", CultureInfo.InvariantCulture)

    let private timeSpan (ts: TimeSpan) = 
        let ms = ts.TotalSeconds
        ms.ToString("#######.000", CultureInfo.InvariantCulture)

    let private replace (pattern: string) (repl: string) (input: string) = 
        Regex.Replace(input, pattern, repl)

    // tbd: trace formatting is broken. We need a proper formatter here! Probably Newtonsoft.Json with
    // some F# extensions. The only requirement is that individual steps should be printed on one line, 
    // or at least for most cases.

    let private postProcess (str: string) = 
        str 
        |> replace "\n" "" // newlines
        |> replace ";[\s]+" "; " // indents
        // option
        |> replace "Some \((.*)\)" "$1"

    module Format =

        // sprintf requires the following types to be public!
        /// Internally used for formatting purposes only, do not use!
        [<NoComparison>]
        type Header = { time: string; param: obj }
            
        /// Formats a TraceHeader to a human comprehensible format. Note that this format might change.
        let header (startTime, param) = 
            { time = dateTime startTime; param = param }
            |> sprintf "%A" |> postProcess

        /// Internally used for formatting purposes only, do not use!
        [<NoComparison>]
        type Step = { offset: string; trace: string }

        /// Formats a TraceStep to a human comprehensible format. Note that this format might change.
        let step (offset, trace) = 
            { offset = timeSpan offset; trace = sprintf "%A" trace }
            |> sprintf "%A" |> postProcess

        [<NoComparison>]
        type Result<'r> = { result: 'r result }

        let result r = 
            { result = r }
            |> sprintf "%A" |> postProcess
        
    /// Formats a Trace to a human comprehensible format. Note that this format might change.
    let trace (Trace((startTime, param), traces, result)) = 
        seq {
            yield Format.header (startTime, param)
            for trace in traces ->
                Format.step trace
            yield Format.result result
        }

module Trace =

    [<AutoOpen>]
    module private Helper = 

        #if false
        let InvalidFilenameChars = Path.GetInvalidFileNameChars()

        let toFilename (str : string) = 
            str.ToCharArray() 
            |> Array.map (fun c -> if Array.IndexOf(InvalidFilenameChars, c) = -1 then c else '_') 
            |> String
        #endif

        let serializer = FsPickler.CreateBinarySerializer()

    let serialize (trace: Trace<'param, 'r>) = 
        use stream = new MemoryStream()
        serializer.Serialize(stream, trace)
        stream.ToArray()

    let deserialize (data: byte[]) : Trace<'param, 'r> =
        use stream = new MemoryStream(data)
        serializer.Deserialize(stream)

module IVR.Tracing

open System
open System.IO
open System.Collections.Generic
open System.Globalization
open System.Diagnostics
open MBrace.FsPickler

//
// IVR Tracing support.
//

#if false
type Name = obj

/// A virtual StartEvent that describes the first initial (start) step of an IVR
type StartEvent = 
    | StartEvent

#endif

/// A step trace represents a trace for a single step of an IVR. 
[<NoComparison>]
type StepTrace = 
    | EventTrace of Event
    | RequestTrace of Request * obj result

[<NoComparison>]
type Trace<'param, 'r> = 
    | Trace of (DateTimeOffset * 'param) * (TimeSpan * StepTrace) list * 'r result

#if false
        
/// A session tracer is a function that consumes trace records for a specific IVR instantiation (session).
type SessionTracer = StepTrace -> unit
    
/// Describes the an IVR tracing session.
[<NoComparison>]
type SessionInfo = { 
    Name: Name
    Id: Id
    Param: obj 
}

/// Creates a new SessionInfo record with a name, an id and a parameterization. 
let sessionInfo name id param = 
    { Name = name; Id = id; Param = param }

/// A Tracer is a function that creates a session tracer for a specific IVR session.
type Tracer = SessionInfo -> SessionTracer

module private Registry = 
    let private _section = obj()

    // Since Name is not comparable, we can't use Map here.
    let mutable private _tracers : Dictionary<Name, Tracer list> = Dictionary<_, _>();

    let addTracer name tracer = 
        lock _section (
            fun () ->
            match _tracers.TryGetValue(name) with
            | true, v -> _tracers.[name] <- tracer::v
            | false, _ -> _tracers.Add(name, [tracer])
        )

    let removeTracer name tracer = 
        lock _section (
            fun () -> 
            match _tracers.TryGetValue(name) with
            | false, _ -> failwithf "Failed to remove a tracer that was previously registered for id %s" (name.ToString())
            | true, tracers ->
                
            let tracers = 
                tracers
                |> List.filter (fun tr -> obj.ReferenceEquals(tr, tracer) |> not)
                    
            if tracers.Length = 0 then
                _tracers.Remove(name) |> ignore
            else
                _tracers.[name] <- tracers
        )
        
    /// Create an session tracer that forwards to all the tracers given.
    let private aggregateTracer tracers sessionInfo : SessionTracer = 
        let itracers = tracers |> List.map (fun t -> t sessionInfo)
        fun stepTrace ->
            itracers |> List.iter (fun st -> st stepTrace)

    /// Returns a single aggregate tracer for the given declared IVR.
    let beginTrace name =
        lock _section (
            fun () ->
            match _tracers.TryGetValue name with
            | false, _ -> None
            | true, tracers ->
                Some (aggregateTracer tracers)
            )

module private SessionIds = 
        
    let private _section = obj()
    let private _generators = Dictionary<Name, Ids.Generator>()

    let generateNew name = 
        lock _section (
            fun () ->
            let g = 
                match _generators.TryGetValue name with
                | false, _ ->
                    let g = Ids.newGenerator()
                    _generators.Add(name, g)
                    g
                | true, g -> g
            g.GenerateId()
        )
       
module private Helper =  
    let disposeAction action = 
        { new IDisposable with
            member this.Dispose() = action() }


#endif

#if false            
    /// Create a host that traces commands. Note that the commands are traced in reversed order.
    let private traceCommands requests host =
        fun request ->
            try
                let r = host request
                requests := { Request = request; Result = Result r } :: !requests
                r
            with e ->
                requests := { Request = request; Result = Error e } :: !requests
                reraise()
#endif

#if false
                    
    let traceResult state = 
        match state with
        | Expecting _
        | Waiting _ -> None
        | Completed r -> r |> Some

    let startAndTraceCommands =
        fun host ivr ->
            let commands = ref []
            let host = traceCommands commands host
            IVR.start host ivr, !commands |> List.rev

    let stepAndTraceCommands =
        fun host event ivr -> 
            let commands = ref []
            let host = traceCommands commands host
            IVR.step event ivr, !commands |> List.rev

    let replayHost commandTraces = 
        let mutable todo = commandTraces
        fun cmd ->
            match todo with
            | [] -> failwith "replay host: no more command traces"
            | cmdt::rest ->
            todo <- rest
            if (cmd <> cmdt.Request) then
                failwithf "replay host: command differs, ignoring result, expected: %A, actual %A" cmdt.Request cmd
            match cmdt.Result with
            | Result r -> r
            | Error e -> raise e
            /// tbd: not sure about this!
            | Cancelled -> failwith "internal error: host can not cancel an IVR"

#endif

#if false

/// Register a tracer for tracing a declared IVR. Whenever the declared IVR is run, the tracer will receive
/// a full trace of the ivr in realtime.

let registerTracer (name: Name) tracer =
    Registry.addTracer name tracer
    Helper.disposeAction (fun () ->
        Registry.removeTracer name tracer
    )

/// Wraps an IVR so that it is traced by the sessionTracer given.
let trace (sessionTracer: SessionTracer) ivr = 
    fun () ->
        let rec next event (state, commands) = 
            { Event = event; Commands = commands; Result = Helper.traceResult state }
            |> sessionTracer
            match state with
            | Completed _ -> 
                state
            | Active _ ->
                fun e -> state |> Helper.stepAndTraceCommands host e |> next e
                |> Active
            | Delayed _ ->
                failwith "internal error state transition from active to inactive observed"

        ivr |> Helper.startAndTraceCommands host |> next StartEvent

#endif

/// Wraps a parameter block and an IVR so that it generates a trace.
let trace (p: 'param) (f : 'param -> 'r ivr) : Trace<'param, 'r> ivr = 

    fun () ->

        let startTime = DateTimeOffset.UtcNow
        let stopwatch = Stopwatch.StartNew()
        
        let rec next traces flux =            
            match flux with
            | Expecting (request, cont) ->
                Expecting (request, 
                    fun response -> 
                        response 
                        |> cont
                        |> next ((stopwatch.Elapsed, RequestTrace (request, response)) :: traces))

            | Waiting (cont) ->
                Waiting (
                    fun event ->
                    event 
                    |> cont 
                    |> next ((stopwatch.Elapsed, EventTrace event) :: traces))

            | Completed r ->
                Trace((startTime, p), traces |> List.rev, r)
                |> Value 
                |> Completed
        
        f p |> IVR.start |> next []

#if false

/// For an IVR to be eligible for tracing by a registered tracer, it must be declared. 
///
/// name is used to identify the IVR and register tracers.
/// f is a function that creates an IVR by passing in a (serializable) parameterization for the IVR.
/// The return value is a function that has the same signature like f, but adds
/// tracing support to it.

let declare (name : Name) (f : 'param -> 'r ivr) = 
    fun param ->
        let ivr = f param

        match Registry.beginTrace name with
        | None -> ivr
        | Some tracer ->
        let id = SessionIds.generateNew name
        let sessionInfo = { Name = name; Id = id; Param = param }
        let sessionTracer = tracer sessionInfo

        ivr 
        |> trace sessionTracer

#endif

#if false

//
// Types for a trace that is annotated with timing information.
//

type TraceHeader = DateTimeOffset * SessionInfo
type TraceStep = TimeSpan * StepTrace
type Trace = TraceHeader * TraceStep list

#endif


#if false
/// Describes the individual differences of a StepTrace
[<RequireQualifiedAccess>]
type StepTraceDiff = 
    | None
    | Event
    | RequestOrResponse 
    | Type

[<NoComparison>]
type StepTraceReport = 
    | StepTraceReport of StepTraceDiff * StepTrace * StepTrace 
    member this.Diff = this |> fun (StepTraceReport (diff, _, _)) -> diff

[<NoComparison>]
type ReplayReport = 
    | ReplayReport of StepTraceReport list
    member this.Steps = this |> fun (ReplayReport steps) -> steps
    member this.Incidents = 
        this.Steps
        |> Seq.filter (fun step -> step.Diff <> StepTraceDiff.None)
    member this.IsEmpty =
        this.Incidents |> Seq.isEmpty

#endif

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ReplayReport<'r> = 
    /// StepTrace can not applied to the current flux, or the list of step traces are empty.
    | Diverged of pending: StepTrace list * current: 'r flux
    | ResultDiffers of expected: 'r result * actual: 'r result
    | Completed

#if false

let private diffStepTrace (expected: StepTrace) (actual: StepTrace) = 

    let diff =
        match expected, actual with
        | _ when expected = actual -> StepTraceDiff.None
        | RequestTrace _, RequestTrace _ -> StepTraceDiff.RequestOrResponse 
        | EventTrace _, EventTrace _ -> StepTraceDiff.Event
        | _ -> StepTraceDiff.Type

    StepTraceReport (diff, expected, actual)

#endif

/// Replay a trace to an IVR and return a report.
let replay (f: 'param -> 'r ivr) (Trace((_, param), traces, expectedResult)) : ReplayReport<'r> = 
    
    let rec next traces flux = 
        match flux with
        | Expecting (request, cont) ->
            match traces with
            | RequestTrace (r, response) :: traces when r = request ->
                response |> cont |> next traces
            | traces -> ReplayReport.Diverged(traces, flux)
        | Waiting (cont) ->
            match traces with
            | EventTrace e :: traces ->
                e |> cont |> next traces
            | traces -> ReplayReport.Diverged(traces, flux)
        | Completed r ->
            match traces with
            | [] when r = expectedResult ->
                ReplayReport.Completed
            | [] ->
                ReplayReport.ResultDiffers(expectedResult, r)
            | traces ->
                ReplayReport.Diverged(traces, flux) 

    f param 
    |> IVR.start 
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

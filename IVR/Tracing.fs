namespace IVR

open System
open System.Collections.Generic
open System.Globalization

//
// IVR Tracing support.
//

module Tracing = 

    type Name = obj

    /// A virtual StartEvent that describes the first initial (start) step of an IVR
    type StartEvent = StartEvent

    /// Represents a trace of a result, either a command invocation result, or the result of an IVR.
    type ResultTrace = 
        | Result of obj
        | Error of exn

    /// Represents a trace of a command. A command can either return without a result or throw an exception.
    type CommandTrace = { command: Command; result: ResultTrace }

    /// A step trace represents a trace for a single step of an IVR. 
    ///
    /// id is a process unque id representing the instance of the IVR that is currently being traced.
    /// event is set to StartupEvent for the first step.
    /// result is set for the last step.

    type StepTrace = { event: Event; commands: CommandTrace list; result: ResultTrace option }
        with
        member this.hasResult = this.result.IsSome

    /// A session tracer is a function that consumes trace records for a specific IVR instantiation (session).
    type SessionTracer = StepTrace -> unit
    
    /// Describes the an IVR tracing session.
    type SessionInfo = { name: Name; id: Id; param: obj }

    /// Creates a new SessionInfo record with a name, an id and a parameterization. 
    let sessionInfo name id param = 
        { name = name; id = id; param = param }

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
                | false, _ -> _tracers.Add(name, tracer::[])
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
                g.generateId()
            )
       
    module private Helper =  
        let disposeAction action = 
            { new IDisposable with
                member this.Dispose() = action() }
            
        /// Create a host that traces commands. Note that the commands are traced in reversed order.
        let private traceCommands commands host =
            fun command ->
                try
                    let r = host command
                    commands := { command = command; result = Result r } :: !commands
                    r
                with e ->
                    commands := { command = command; result = Error e } :: !commands
                    reraise()
                    
        let traceResult state = 
            match state with
            | Active _ -> None
            | Completed (IVR.Result r) -> Result r |> Some
            | Completed (IVR.Error e) -> Error e |> Some

        let startAndTraceCommands =
            fun host ivr ->
                let commands = ref []
                let host = traceCommands commands host
                IVR.start host ivr, !commands |> List.rev

        let stepAndTraceCommands =
            fun host event ivr -> 
                let commands = ref []
                let host = traceCommands commands host
                IVR.step host event ivr, !commands |> List.rev

        let replayHost commandTraces = 
            let mutable todo = commandTraces
            fun cmd ->
                match todo with
                | [] -> failwith "replay host: no more command traces"
                | cmdt::rest ->
                todo <- rest
                if (cmd <> cmdt.command) then
                    failwithf "replay host: command differs, ignoring result, expected: %A, actual %A" cmdt.command cmd
                match cmdt.result with
                | Result r -> r
                | Error e -> raise e

    /// Register a tracer for tracing a declared IVR. Whenever the declared IVR is run, the tracer will receive
    /// a full trace of the ivr in realtime.

    let registerTracer (name: Name) tracer =
        Registry.addTracer name tracer
        Helper.disposeAction (fun () ->
            Registry.removeTracer name tracer
        )

    /// Wraps an IVR so that it is traced by the sessionTracer given.
    let trace sessionTracer ivr = 
        fun host ->
            let rec next event (state, commands) = 
                { event = event; commands = commands; result = Helper.traceResult state }
                |> sessionTracer
                match state with
                | Completed _ -> 
                    state
                | Active _ ->
                    fun e h -> state |> Helper.stepAndTraceCommands h e |> next e
                    |> Active

            ivr |> Helper.startAndTraceCommands host |> next StartEvent

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
            let sessionInfo = { name = name; id = id; param = param }
            let sessionTracer = tracer sessionInfo

            ivr 
            |> trace sessionTracer

    //
    // Types for a trace that is annotated with timing information.
    //

    type TraceHeader = DateTime * SessionInfo
    type TraceStep = TimeSpan * StepTrace
    type Trace = TraceHeader * TraceStep list


    /// Describes the individual differences of a StepTrace
    [<Flags>]
    type StepTraceDiff = 
        | None = 0x0
        | Event = 0x1
        | Commands = 0x2 
        | Result = 0x4

    type StepTraceReport = StepTraceReport of StepTraceDiff * StepTrace * StepTrace 
        with
        member this.Diff = this |> fun (StepTraceReport (diff, _, _)) -> diff

    type ReplayReport = ReplayReport of StepTraceReport list
        with 
        member this.steps = this |> fun (ReplayReport steps) -> steps
        member this.incidents = 
            this.steps
            |> Seq.filter (fun step -> step.Diff <> StepTraceDiff.None)
        member this.isEmpty =
            this.incidents |> Seq.isEmpty

    let private diffStepTrace (expected: StepTrace) (actual: StepTrace) = 
        let none = StepTraceDiff.None
        let diff =
            if (expected.event <> actual.event) then StepTraceDiff.Event else none
            |||
            if (expected.commands <> actual.commands) then StepTraceDiff.Commands else none
            |||
            if (expected.result <> actual.result) then StepTraceDiff.Result else none

        StepTraceReport (diff, expected, actual)

    /// Replay a trace to an IVR and return a report.
    let replay (f: 'param -> 'r ivr) (trace: Trace) : ReplayReport = 
        let sessionInfo = trace |> fst |> snd
        let stepTraces = trace |> snd |> List.map snd
        let ivr = unbox sessionInfo.param |> f
        
        let rec next report stepTraces (state, commands) = 
            match stepTraces with
            | [] -> failwith "internal error, a trace must have at least one StepTrace with a StartEvent"
            | step :: rest ->
            let actual = { event = step.event; commands = commands; result = Helper.traceResult state}
            let report = diffStepTrace step actual :: report
            match state with
            | Completed _ -> report
            | _ ->
            match rest with
            | [] -> report // premature end
            | nextStep :: _ ->

            let replayHost = Helper.replayHost nextStep.commands 
            state
            |> Helper.stepAndTraceCommands replayHost nextStep.event 
            |> next report rest
        
        ivr 
        |> Helper.startAndTraceCommands (stepTraces |> List.head |> (fun st -> st.commands) |> Helper.replayHost)
        |> next [] stepTraces 
        |> List.rev 
        |> ReplayReport


    /// Module to convert traces into a human comprehensible format.
    module Format =

        open System.Text.RegularExpressions

        let private dateTime (dt: DateTime) = 
            // Inspired by ISO 8601, 
            // but changed T->_, removed date und time separators, and 3 fractional seconds for presenting milliseconds.
            dt
                .ToLocalTime()
                .ToString("yyyyMMdd_HHmmss.fff", CultureInfo.InvariantCulture)

        let private timeSpan (ts: TimeSpan) = 
            let ms = ts.TotalSeconds
            ms.ToString("#######.000", CultureInfo.InvariantCulture)

        // sprintf requires the following types to be public!
        /// Internally used for formatting purposes only, do not use!
        type Header = { time: string; name: Name; id: Id; param: obj }
        /// Internally used for formatting purposes only, do not use!
        type Step = { offset: string; event: Event; commands: CommandTrace list; result: ResultTrace option }

        let private replace (pattern: string) (repl: string) (input: string) = 
            Regex.Replace(input, pattern, repl)

        // tbd: trace formatting is broken. We need a proper formatter here! Probably Newtonsoft.Json with
        // some F# extensions. The only requirement is that individual steps should be printed on one line, 
        // or at least for most cases.

        let private postProcess (str: string) = 
            str 
            |> replace "\n" "" // newlines
            |> replace ";[\s]+" "; " // indents
            // defaults
            |> replace " result = null;" ""
            |> replace " param = null;" ""
            |> replace " error = null;" ""
            |> replace " commands = \[\];" ""
            |> replace " name = \"\";" ""
            |> replace " id = 0L;" ""
            // option
            |> replace "Some \((.*)\)" "$1"
            
        /// Formats a TraceHeader to a human comprehensible format. Note that this format might change.
        let header (header: TraceHeader) = 
            let (time, si) = header
            { time = dateTime time; name = si.name; id = si.id; param = si.param }
            |> sprintf "%A" |> postProcess

        /// Formats a TraceStep to a human comprehensible format. Note that this format might change.
        let step (step: TraceStep) = 
            let (offset, step) = step
            { offset = timeSpan offset; event = step.event; commands = step.commands; result = step.result }
            |> sprintf "%A" |> postProcess
 
        /// Formats a Trace to a human comprehensible format. Note that this format might change.
        let trace (trace: Trace) = 
            seq {
                yield header (fst trace)
                for s in (snd trace) ->
                    step s
            }

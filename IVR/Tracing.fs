namespace IVR

open System
open System.Collections.Generic

//
// IVR Tracing support.
//

module Tracing = 

    type Name = obj

    /// A virtual StartEvent that descripes the first initial (start) step of an IVR
    type StartEvent = StartEvent

    /// Represents a trace of a command. A command can either return without a result or throw an exception.
    type CommandTrace = { command: Command; error: exn option }

    type ResultTrace = 
        | Result of obj
        | Error of exn

    /// A step trace represents a trace for a single step of an IVR. 
    ///
    /// id is a process unque id representing the instance of the IVR that is currently being traced.
    /// event is set to StartupEvent for the first step.
    /// result is set for the last step.

    type StepTrace = { event: Event; commands: CommandTrace list; result: ResultTrace option }
    type Trace = StepTrace list

    /// A session tracer is a function that consumes Trace records for a specific IVR instantiation (session).
    type SessionTracer = StepTrace -> unit
    
    type SessionInfo = { name: Name; id: Id; param: obj }
 
    /// A tracer creates a session trace for a specific ivr session.
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
            
        /// Create a host that traces commands
        let private traceCommands commands host =
            fun command ->
                let error = 
                    try
                        host command
                        None
                    with e ->
                        Some e
                commands := { command = command; error = error } :: !commands

        let private traceResult r = 
            match r with
            | Active _ -> None
            | Completed (IVR.Result r) -> Result r |> Some
            | Completed (IVR.Error e) -> Error e |> Some

        let traceStart sessionTracer =
            fun host ivr ->
                let commands = ref []
                let host = traceCommands commands host
                let r = IVR.start host ivr
                { event = StartEvent; commands = !commands; result = traceResult r }
                |> sessionTracer
                r

        let traceStep sessionTracer =
            fun host event ivr -> 
                let commands = ref []
                let host = traceCommands commands host
                let r = IVR.step host event ivr
                { event = event; commands = !commands; result = traceResult r }
                |> sessionTracer
                r

    /// Register a tracer for tracing a specific declared IVR. Whenever the declared IVR is run, the tracer will receive
    /// a full trace of the ivr in realtime.

    let registerTracer (name: Name) tracer =
        Registry.addTracer name tracer
        Helper.disposeAction (fun () ->
            Registry.removeTracer name tracer
        )

    /// Traces an IVR.
    let trace sessionTracer ivr = 
        fun host ->
            let step = Helper.traceStep sessionTracer

            let rec next ivr = 
                match ivr with
                | Completed _ -> ivr
                | Active _ ->
                fun e h ->
                    ivr |> step h e |> next
                |> Active

            ivr |> Helper.traceStart sessionTracer host |> next

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

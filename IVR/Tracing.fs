namespace IVR

open System
open System.Collections.Generic

//
// IVR Tracing support.
//

module Tracing = 

    type DateTimeUTC = DateTimeUTC of DateTime

    type Name = obj

    /// A virtual StartupEvent that descripes the first startup step of an IVR
    type StartupEvent = StartupEvent

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

    type StepTrace = { name: Name; id: Id; time: DateTimeUTC; event: Event; commands: CommandTrace list; result: ResultTrace option }
    type Trace = StepTrace list

    /// A instance tracer is a function that consumes Trace records for a specific IVR instance.
    type InstanceTracer = StepTrace -> unit

    /// A tracer creates a instance trace for a specific ivr id.
    type Tracer = Name -> InstanceTracer

    module private Global = 
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
           
    module private Helper =  
        let disposeAction action = 
            { new IDisposable with
                member this.Dispose() = action() }

    let beginTracing (ivrName: Name) tracer =
        Global.addTracer ivrName tracer
        Helper.disposeAction (fun () ->
            Global.removeTracer ivrName tracer
        )
    
    /// For an IVR to be eligable for tracing, it must be declared. The instance given is to 
    /// associate the IVR, it can be a string or any other instance, like a union cases for example.
    #if false
    let declare name ivr = 

        fun () ->
            let tracer = beginTrace instance

            let rec next ivr = 
                match ivr with
                | Active _ -> Active (fun e -> tracer.traceEvent e; (ivr |> IVR.step e |> next))
                | Completed r ->
                    tracer.traceResult r
                    ivr

            ivr |> IVR.start |> next
        |> Delay


    #endif

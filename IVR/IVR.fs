namespace IVR

open System
open System.Threading

open Threading

(*
    An IVR is a definition of an asynchronous process with the following properties:
    
    - can be ended at any given time without a concrete reason.
    - can only synchronously respond to events. 
*)

type Event = obj

type IVR<'result> = 
    | Delay of (unit -> IVR<'result>)
    | Active of (Event -> IVR<'result>)
    | Completed of 'result

type 'result ivr = IVR<'result>

module IVR = 

    // start up this ivr 
    // this goes through all Delay states and stops at Active or Completed.

    let rec start ivr = 
        match ivr with
        | Delay f -> f() |> start
        | _ -> ivr

    let step ivr e = 
        match ivr with
        // may be we should start it here, too?
        //> no: delays are not supported, once an IVR starts, subsequential
        //> IVRs do have to be started before stepping through

        | Delay _ -> failwith "IVR.step: ivr not started"
        | Completed _ -> failwith "IVR.step: ivr is completed"
        | Active f -> f e

    // note that step would complain about a Completed state
    let private progress ivr e = 
        match ivr with 
        | Completed _ -> ivr
        | Active f -> f e 
        | Delay _ -> failwithf "IVR.lpar: seen unexpected Delay"

    let isCompleted ivr = 
        match ivr with
        | Completed _ -> true
        | _ -> false

    let result ivr = 
        match ivr with
        | Completed r -> r
        | _ -> failwith "IVR.result: ivr is not completed"

    // run two ivrs in parallel, the resulting ivr completes, when both ivrs are completed.
    // events are delivered first to ivr1, then to ivr2.

    // note that par retains the result of the completed ivr, which
    // could lead to leaks in nested parallel ivrs of which the result
    // is never processed
    // to avoid such leaks, use 

    let par (ivr1 : IVR<'r1>) (ivr2 : IVR<'r2>) : IVR<'r1 * 'r2> =

        let rec active ivr1 ivr2 e = 
            next (progress ivr1 e) (progress ivr2 e)

        and next ivr1 ivr2 =
            match ivr1, ivr2 with
            | Completed r1, Completed r2 -> Completed (r1, r2)
            | _ -> active ivr1 ivr2 |> Active

        fun () -> next (start ivr1) (start ivr2)
        |> Delay

    // combine a list of ivrs so that they run in parallel

    let lpar (ivrs: 'r ivr list) : 'r list ivr = 

        let rec active ivrs e =
            let ivrs = 
                ivrs
                |> List.map (fun ivr -> progress ivr e)
            next ivrs

        and next ivrs = 
            let anyActive = 
                ivrs
                |> List.exists (isCompleted >> not)
            match anyActive with
            | true -> active ivrs |> Active
            | false ->
                ivrs
                |> List.map result
                |> Completed

        fun () -> next (List.map start ivrs)
        |> Delay

    // specialized version of lpar that removes results from processing when
    // the return type is unit.
    // tbd
    // lpar_ 

    // run two ivrs in parallel, the resulting ivr completes with the result of the one that finishes first.
    // events are delivered to ivr1 and then to ivr2, so ivr1 has an advantage when both complete in response to
    // the same event. Note that if ivr1 completes, no event is delivered to ivr2.
    
    let par' (ivr1 : IVR<'r1>) (ivr2 : IVR<'r2>) : IVR<Choice<'r1, 'r2>> =

        let rec loop ivr1 ivr2 e = 

            match ivr1, ivr2 with
            | Active f1, Active f2 -> 
                let ivr1 = f1 e
                match ivr1 with
                | Completed r1 -> Completed <| Choice1Of2 r1
                | _ ->
                let ivr2 = f2 e
                match ivr2 with
                | Completed r2 -> Completed <| Choice2Of2 r2
                | _ -> Active <| loop ivr1 ivr2
            | _ -> failwithf "par': unexpected %A, %A" ivr1 ivr2

        fun () -> 
            let ivr1 = start ivr1
            match ivr1 with
            | Completed r1 -> Completed <| Choice1Of2 r1
            | _ ->
            let ivr2 = start ivr2
            match ivr2 with
            | Completed r2 -> Completed <| Choice2Of2 r2
            | _ ->
            Active <| loop ivr1 ivr2
            
        |> Delay

    // runs a list of ivrs in parallel and finish with the first one that completes.

    let lpar' (ivrs: 'r ivr list) : 'r ivr =

        let folder f (ivrs, result) ivr = 
            // don't step pending ivrs, if we already have a result!
            // also: this is a kind of idle part of this algorithm, actually
            // we need to break the folder as soon we see a Completed ivr
            match result with
            | Some r -> [], Some r
            | None ->
            let ivr = f ivr
            match ivr with
            | Active _ -> (ivr::ivrs), None
            | Completed r -> [], Some r
            | Delay _ -> failwith "IVR.lpar`: unexpected Delay"

        let stepFolder e = folder (fun ivr -> step ivr e)
        let startFolder = folder start

        let rec active ivrs e = 

            ivrs 
            |> List.fold (stepFolder e) ([], None)
            |> next
           
        and next (ivrs, result) = 
            match result with
            | Some r -> Completed r
            | None -> Active (active (ivrs |> List.rev))

        fun () ->
            ivrs
            |> List.fold startFolder ([], None)
            |> next

        |> Delay

    //
    // more basic primitives
    //

    // wait for some event

    let wait predicate =
        let rec waiter e =  
            match predicate e with
            | Some r -> Completed r
            | None -> Active waiter

        Active waiter

    // wait for any event

    let waitAny = wait (fun _ -> Some ())

    // wait for some event with a predicate that returns
    // true or false

    let wait' predicate = 
        let f e = 
            let r = predicate e
            match r with 
            | true -> Some ()
            | false -> None

        wait f

    // wait for an event of a type derived by a function passed in

    let waitFor (f : 'e -> 'r) = 

        let f (e : Event) = 
            match e with
            | :? 'e as e -> f e |> Some
            | _ -> None

        wait f

    let waitFor' (f : 'e -> bool) =
        let f (e: Event) =
            match e with
            | :? 'e as e -> f e
            | _ -> false

        wait' f
        
    //
    // The ivr host, where all events are being dispatched
    // 

    type Id = int64
    let private id = ref 0L
    let private newId() = Interlocked.Increment(id)

    // predefined host events


    type Timeout = Timeout of Id
    type CancelIVR = CancelIVR

    type Host = { queue: SynchronizedQueue<obj> }

        with 
        member this.dispatch event = 
            this.queue.enqueue event

        member this.cancel() = 
            this.dispatch CancelIVR

        member this.delay (timespan : TimeSpan) = 
            let id = newId()
            let callback _ = this.dispatch (Timeout id)
            let timer = new Timer(callback, null, int64 timespan.TotalMilliseconds, -1L)
            waitFor' (fun (Timeout tid) -> tid = id)
            
    let newHost() = { queue = SynchronizedQueue() }

    // Synchronously runs an ivr on a host
    // Returns Non if the ivr was interrupted
    
    let run ivr host = 
        
        let rec runLoop ivr = 
            let event = host.queue.dequeue()
            match event with
            | :? CancelIVR -> None
            | event ->
            let ivr = step ivr event
            match ivr with
            | Completed r -> Some r
            | Active _ -> runLoop ivr
            | Delay _ -> failwithf "IVR.run: unexpected: %A" ivr

        runLoop ivr

    (*
        Simple computation expression, to build sequential IVR processes by
        continuations / monads.
    *)

    type IVRBuilder<'result>() = 
        member this.Bind(ivr: IVR<'r>, cont: 'r -> IVR<'r2>) : IVR<'r2> = 

            let rec loop (ivr:IVR<'r>) e =
                match ivr with
                | Delay _ -> failwith "bind: unexpected Delay"
                | Active f -> 
                    let i = f e
                    Active (loop i)
                | Completed r -> cont r
            
            Active (loop ivr)

        member this.Return(v) = Completed v

        member this.ReturnFrom ivr = start ivr

        // We want to delay the startup of an IVR to the moment IVR.start is run, because
        // computation expressions may contain regular code at the beginning that would run at
        // instantiation of the expression and not when we start / run the ivr
        member this.Delay (f : unit -> IVR<'r>) : IVR<'r> = Delay f

        // zero makes only sense for IVR<unit>
        member this.Zero () = Completed ()

    let ivr<'result> = IVRBuilder<'result>()

    let map f ivr' = 
        ivr {
            let! r = ivr'
            return f r
        }

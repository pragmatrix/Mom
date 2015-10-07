namespace IVR

open System

(*
    An IVR is a definition of an asynchronous process with the following properties:
    
    - can be paused and ended at any time.
    - can synchronously wait and respond to events.
    - can be combined in parallel or sequentially.
    - can send any number of commands to a host.
    - can be run in individual steps.
*)

type Event = obj
type Command = obj
type Response = obj
type Host = Command -> Response

exception Cancelled

type Result<'result> =
    | Result of 'result
    | Error of Exception
    with 
        member this.map f =
            match this with
            | Error e -> Error e
            | Result r -> Result (f r)

type IVRState<'result> = 
    | Active of (Event -> IVR<'result>)
    | Completed of Result<'result>

and IVR<'result> = Host -> IVRState<'result>

type 'result ivr = IVR<'result>

module TimeSpanExtensions =

    type Int32 with
        member this.seconds = TimeSpan.FromSeconds(float this)
        member this.milliseconds = TimeSpan.FromMilliseconds(float this)

    type Double with
        member this.seconds = TimeSpan.FromSeconds(this)
        member this.milliseconds = TimeSpan.FromMilliseconds(this)
        

module IVR = 

    // 
    // IVR Primitives Part 1
    //

    /// Start up an ivr.
    let start host ivr = 
        try
            ivr host
        with e ->
            e |> Error |> Completed

    /// Continue an active ivr with one event.
    let step h e ivr = 
        match ivr with
        // may be we should start it here, too?
        //> no: delays are not supported, once an IVR starts, subsequential
        //> IVRs do have to be started before stepping through

        | Completed _ -> failwithf "IVR.step: ivr is completed: %A" ivr
        | Active f -> 
            try
                f e h
            with e ->
                e |> Error |> Completed

    /// Step an active ivr, otherwise do nothing.
    let private stepWhenActive h e ivr = 
        match ivr with 
        | Active _ -> step h e ivr
        | _ -> ivr
        
    /// Returns true if the ivr is completed (i.e. has a result).
    let isCompleted ivr = 
        match ivr with
        | Completed _ -> true
        | _ -> false

    let isActive ivr = 
        match ivr with
        | Active _ -> true
        | _ -> false

    let isError ivr = 
        match ivr with
        | Completed (Error _) -> true
        | _ -> false

    let (|IsError|_|) ivr = 
        match ivr with
        | Completed (Error e) -> Some e
        | _ -> None

    /// Returns the error of a completed ivr.
    let error ivr = 
        match ivr with
        | Completed (Error e) -> e
        | _ -> failwithf "IVR.error: ivr is not in error: %A" ivr

    let isCancelled ivr = 
        match ivr with
        | Completed (Error Cancelled) -> true
        | _ -> false

    let (|IsCancelled|_|) ivr = 
        if isCancelled ivr then Some() else None

    //
    // Primitives Part 2
    //

    /// Continues the ivr with a followup ivr (Monad bind).
    let continueWith (f : Result<'a> -> IVR<'b>) (ivr: IVR<'a>) : IVR<'b> =

        let rec next h state = 
            match state with
            | Active _ -> 
                fun e h -> state |> step h e |> next h
                |> Active
            | Completed r -> 
                f r |> start h

        fun h ->
            ivr |> start h |> next h

    /// Maps the ivr's result.
    let map (f: 'a -> 'b) (ivr: IVR<'a>) : IVR<'b> = 
        let f (r: Result<'a>) = fun _ -> r.map f |> Completed
        continueWith f ivr

    /// Ignores the ivr's result type.
    let ignore ivr = ivr |> map ignore
            
    /// Invokes a function when the ivr is completed.
    let whenCompleted f =
        continueWith (fun r -> f(); fun _ -> r |> Completed)

    /// Returns the result of a completed ivr.
    let result ivr = 
        match ivr with
        | Completed r -> r
        | _ -> failwithf "IVR.result: ivr is not completed: %A" ivr

    /// Returns the resulting value of a completed ivr.
    let resultValue ivr = 
        match ivr with
        | Completed (Result r) -> r
        | Completed _ -> failwithf "IVR.resultValue ivr has not been completed with a resulting value: %A" ivr
        | _ -> failwithf "IVR.resultValue: ivr is not completed: %A" ivr

    /// The event that is sent to an active IVR when it gets cancelled. The only accepted return state is Cancelled.
    type Cancel = Cancel

    /// Cancels the ivr. For now it is expected that the ivr immediately returns the Cancelled state.
    let cancel h ivr = 
        match ivr with
        | Active _ -> 
            let r = ivr |> step h Cancel
            match r with
            | IsCancelled -> ()
            | _ -> failwithf "IVR.cancel: cancellation unhandled: %A" r
        | _ -> failwith "IVR.cancel: ivr is not active"

    let cancelIfActive h ivr = 
        if isActive ivr then cancel h ivr

    //
    // IVR Combinators
    //

    /// Combine a list of ivrs so that they run in parallel. The resulting ivr ends when 
    /// All ivrs ended. When an error occurs in one of the ivrs, the resulting ivr ends with
    /// that error.

    let lpar (ivrs: 'r ivr list) : 'r list ivr = 

        // Cancellation is always in reverse order!
        let cancelAllActive h ivrs = 
            ivrs
            |> List.rev 
            |> List.iter (cancelIfActive h)

        let rec active ivrs e h =
            ivrs
            |> List.map (stepWhenActive h e)
            |> next h
            
        and next h ivrs = 
            let anyError = 
                ivrs
                |> List.exists (isError)

            let anyActive = 
                ivrs
                |> List.exists (isActive)

            match anyError, anyActive with
            | true, _ -> 
                cancelAllActive h ivrs
                // return the first error, but since we step them all in parallel, multiple 
                // errors may be found, and we should accumulate them.
                // (better would be to change the semantic to only step one at a time and terminate on the first error seen)
                ivrs |> List.find (isError) |> error |> Error |> Completed
            | false, true -> active ivrs |> Active
            | false, false ->
                ivrs
                |> List.map resultValue
                |> Result
                |> Completed

        fun h -> next h (List.map (start h) ivrs)

    /// Runs two ivrs in parallel, the resulting ivr completes, when both ivrs are completed.
    /// Events are delivered first to ivr1, then to ivr2. When one of the ivrs terminates without a result 
    /// (cancellation or exception),
    /// the resulting ivr is ended immediately.

    /// Note that par retains the result of the completed ivr, which
    /// could lead to leaks in nested parallel ivrs of which the result
    /// is never processed.

    let par (ivr1 : IVR<'r1>) (ivr2 : IVR<'r2>) : IVR<'r1 * 'r2> =

        // we implement par in terms of lpar so that we avoid
        // the maintainance of two semantics

        [map box ivr1; map box ivr2] 
        |> lpar
        |> map (function 
            | [l;r] -> unbox l, unbox r 
            | _ -> failwith "internal error: here to keep the compiler happy")


    // specialized version of lpar that removes results from processing when
    // the return type is unit.
    // tbd
    // lpar_ 

    /// Runs two ivrs in parallel, the resulting ivr completes with the result of the one that finishes first.
    /// events are delivered to ivr1 and then to ivr2, so ivr1 has an advantage when both complete in response to
    /// the same event. Note that if ivr1 completes, no event is delivered to ivr2.
    
    let par' (ivr1 : IVR<'r1>) (ivr2 : IVR<'r2>) : IVR<Choice<'r1, 'r2>> =

        let rec loop ivr1 ivr2 e h = 

            match ivr1, ivr2 with
            | Active _, Active _ -> 
                ivr1
                |> step h e
                |> function
                | Completed r1 -> 
                    cancel h ivr2
                    r1.map Choice1Of2 |> Completed
                | ivr1 ->
                ivr2 
                |> step h e
                |> function
                | Completed r2 -> 
                    cancel h ivr1
                    r2.map Choice2Of2 |> Completed
                | ivr2 ->
                    loop ivr1 ivr2 |> Active
            | _ -> failwithf "IVR.par': unexpected %A, %A" ivr1 ivr2

        fun h -> 
            ivr1
            |> start h
            |> function
            | Completed r1 -> r1.map Choice1Of2 |> Completed
            | ivr1 ->
            ivr2 
            |> start h
            |> function
            | Completed r2 ->
                cancel h ivr1 
                r2.map Choice2Of2 |> Completed
            | ivr2 ->
                loop ivr1 ivr2 |> Active

    /// Runs a list of ivrs in parallel and finish with the first one that completes.

    let lpar' (ivrs: 'r ivr list) : 'r ivr =

        let rec startAll h res active ivrs = 
            match res with
            | Some _ -> res, []
            | _ ->
            match ivrs with
            | [] -> res, (active |> List.rev)
            | ivr::todo ->
                ivr
                |> start h
                |> function
                | Completed r -> 
                    // cancel all the running ones in reversed order 
                    // (which is how they stored until returned)
                    active |> List.iter (cancel h)
                    Some r, []
                | Active _ as ivr -> 
                    startAll h None (ivr::active) todo

        let rec stepAll h e res active ivrs =
            match res with
            | Some _ -> res, []
            | _ ->
            match ivrs with
            | [] -> res, (active |> List.rev)
            | ivr::todo ->
                ivr
                |> step h e
                |> function
                | Completed r ->
                    // cancel all the running ones in reversed order
                    // and the future ones (also in reversed order)
                    active |> List.iter (cancel h)
                    todo |> List.rev |> List.iter (cancel h)
                    Some r, []
                | Active _ as ivr ->
                    stepAll h e None (ivr::active) todo

        let rec active ivrs e h = 
            ivrs 
            |> stepAll h e None []
            |> next h
           
        and next h (result, ivrs) = 
            match result with
            | Some r -> r |> Completed
            | None -> Active (active ivrs)

        fun h ->
            ivrs
            |> startAll h None []
            |> next h

    //
    // Sending and posting commands to the host.
    //

    /// Response type interface that is used to tag commands with.
    type IReturns<'result> = 
        interface end

    /// An IVR that synchronously sends a command to a host and returns its response.
    let send (cmd: IReturns<'r>) : 'r ivr = 
        fun h ->
            try
                cmd
                |> h
                |> unbox 
                |> Result |> Completed
            with e ->
                e |> Error |> Completed

    /// An IVR that Synchronously sends a command to a host, but ignores its response.
    let post cmd : unit ivr = 
        fun (h:Host) ->
            try
                cmd |> h |> Operators.ignore
                () |> Result |> Completed
            with e ->
                e |> Error |> Completed

    //
    // Simple computation expression to build sequential IVR processes
    //

    type IVRBuilder<'result>() = 
        member this.Bind(ivr: 'r ivr, cont: 'r -> 'r2 ivr) : 'r2 ivr = 
            ivr
            |> continueWith (
                function 
                | Result r -> cont r
                | Error err -> 
                    fun _ -> err |> Error |> Completed)

        member this.Return v = fun _ -> v |> Result |> Completed

        member this.ReturnFrom ivr = ivr

        member this.Delay (f : unit -> 'r ivr) : 'r ivr =
            fun h ->
                f()
                |> start h
              
        member this.Zero () = 
            fun _ -> () |> Result |> Completed

        member this.Using(disposable : 't, body : 't -> 'u ivr when 't :> IDisposable) : 'u ivr = 
            body disposable
            |> whenCompleted disposable.Dispose

        member this.TryFinally (ivr: 'r ivr, f: unit -> unit) : 'r ivr =
            ivr
            |> whenCompleted f

        member this.TryWith (ivr: 'r ivr, eh: exn -> 'r ivr) : 'r ivr =
            ivr
            |> continueWith (function
                | Error e -> eh e
                | r -> fun _ -> r |> Completed)

        member this.Yield (cmd: Command) : unit ivr = post cmd

        member this.Combine(ivr1: unit ivr, ivr2: 'r ivr) : 'r ivr =
            ivr1
            |> continueWith (function
                | Error e -> fun _ -> e |> Error |> Completed
                | _ -> ivr2
            )
    
        // http://fsharpforfunandprofit.com/posts/computation-expressions-builder-part6/

        // While can be implemented in terms of Zero() and Bind()
        member this.While(guard: unit -> bool, body: unit ivr) : unit ivr =
            if not (guard()) then
                this.Zero()
            else
                this.Bind(body, fun () ->
                    this.While(guard, body))

        // For with Using(), While(), and Delay().
        member this.For(sequence: seq<'a>, body: 'a -> unit ivr) : unit ivr =
            this.Using(sequence.GetEnumerator(), fun enum ->
                this.While(enum.MoveNext,
                    this.Delay(fun () -> body enum.Current)))

    let ivr<'result> = IVRBuilder<'result>()
            
    //
    // Wait primitives
    //

    /// An IVR that waits for some event given a function that returns (Some result) or None.

    let wait f =
        let rec waiter e _ =  
            match box e with
            | :? Cancel -> Cancelled |> Error |> Completed
            | _ ->
            match f e with
            | Some r -> r |> Result |> Completed
            | None -> Active waiter

        fun _ ->
            Active waiter

    /// Waits for some event with a predicate that returns
    /// true or false

    let wait' predicate = 
        let f e = 
            let r = predicate e
            match r with 
            | true -> Some ()
            | false -> None

        wait f

    /// Waits for an event of a type derived by a function passed in.

    let waitFor (f : 'e -> 'r option) = 

        let f (e : Event) = 
            match e with
            | :? 'e as e -> f e
            | _ -> None

        wait f

    let waitFor' (f : 'e -> bool) =
        let f (e: Event) =
            match e with
            | :? 'e as e -> f e
            | _ -> false

        wait' f

    //
    // IVR System Commands & Events
    //

    type Delay = Delay of TimeSpan
        with
        interface IReturns<Id>

    type SystemEvent = 
        | DelayCompleted of Id

    let delay (ts: TimeSpan) =
        ivr {
            let! id = Delay ts |> send
            do! waitFor' (fun (DelayCompleted id') -> id' = id)
        }

    //
    // TPL naming scheme
    //

    /// Wait for all ivrs to finish.
    let waitAll ivrs = lpar ivrs

    /// Wait for any one of the ivrs to finish.
    let waitAny ivrs = lpar' ivrs 

module BuilderExtensions = 
    let ivr<'r> = IVR.ivr<'r>

// a little inception may not do any harm :)

[<assembly:AutoOpen("IVR.TimeSpanExtensions")>]
[<assembly:AutoOpen("IVR.BuilderExtensions")>]
do ()

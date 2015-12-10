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

[<NoComparison>]
type Result<'result> =
    | Value of 'result
    | Error of exn
    member this.map f =
        match this with
        | Error e -> Error e
        | Value r -> Value (f r)

[<NoComparison;NoEquality>]
type 'result ivr = 
    | Inactive of (Host -> 'result ivr)
    | Active of (Event -> Host -> 'result ivr)
    | Completed of Result<'result>

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
        match ivr with
        | Inactive f -> 
            try
                f host
            with e ->
                e |> Error |> Completed
        | _ -> failwithf "IVR.start: ivr not inactive: %A" ivr

    /// Continue an ivr with one event.
    let rec step h e ivr = 
        match ivr with
        // may be we should start it here, too?
        //> no: delays are not supported, once an IVR starts, subsequential
        //> IVRs do have to be started before stepping through

        | Active f -> 
            try
                f e h
            with e ->
                e |> Error |> Completed
       
        | Inactive _ -> failwithf "IVR.step: ivr is inactive"
        | Completed _ -> failwithf "IVR.step: ivr is completed: %A" ivr
        
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
    let continueWith (f : Result<'a> -> 'b ivr) (ivr: 'a ivr) : 'b ivr =

        let rec next h state = 
            match state with
            | Active _ -> 
                fun e h -> state |> step h e |> next h
                |> Active
            | Completed r -> 
                f r |> start h
            | Inactive _ ->
                failwithf "IVR.continueWith, ivr is inactive: %A" state

        fun h ->
            ivr |> start h |> next h
        |> Inactive

    /// Maps the ivr's result. In other words: lifts a function that converts a value from a to b
    /// into the IVR category.
    let map (f: 'a -> 'b) (ivr: 'a ivr) : 'b ivr = 
        let f (r: Result<'a>) = 
            fun _ -> r.map f |> Completed
            |> Inactive
        continueWith f ivr

    /// Ignores the ivr's result type.
    let ignore ivr = ivr |> map ignore
            
    /// Invokes a function when the ivr is completed.
    let whenCompleted f =
        continueWith (fun r -> f(); (fun _ -> r |> Completed) |> Inactive)

    /// Returns the result of a completed ivr.
    let result ivr = 
        match ivr with
        | Completed r -> r
        | _ -> failwithf "IVR.result: ivr is not completed: %A" ivr

    /// Returns the resulting value of a completed ivr.
    let resultValue ivr = 
        match ivr with
        | Completed (Value r) -> r
        | Completed _ -> failwithf "IVR.resultValue ivr has not been completed with a resulting value: %A" ivr
        | _ -> failwithf "IVR.resultValue: ivr is not completed: %A" ivr

    /// The event that is sent to an active IVR when it gets cancelled. The only accepted return state is Cancelled.
    type Cancel = Cancel

    /// Tries to cancels the ivr. This actually sends a Cancel event to the ivr, but otherwise
    /// does nothing. The ivr is responsible to react on Cancel events (for example every wait function does that).
    /// Also note that ivrs can continue after cancellation was successful. For example finally
    /// handlers can run ivrs.
    /// If an ivr is completed, the result is overwritten (freed indirectly) with a Cancelled error,
    /// but an error is not, to avoid shadowing the error.
    /// If an ivr is Inactive, it is also converted to Cancelled error, to prevent anyone else
    /// from activating it and to free the code behind the activation function.
    let tryCancel h ivr = 
        match ivr with
        | Active _ -> 
            ivr |> step h Cancel
        | Completed (Error _) -> ivr
        | _ -> Cancelled |> Error |> Completed

    //
    // IVR Combinators
    //

    // Cancels a pair of ivr lists in the reversed specified order while in the process of 
    // processing a step for all parallel ivrs.
    // active are the ones that where already processed (in the reversed order specified).
    // todo are the ones that have not yet processed for this round (in the specified order).
    // Returns the ivrs that stay active after the cancellation was sent.

    let private parCancel h active todo = 
        let todo = todo |> List.rev
        todo @ active
        |> List.map (tryCancel h) 
        |> List.filter isActive 
        |> List.rev         


    let private parCancel' h active todo = 
        let cancelIVRs ivrs = ivrs |> List.map(tryCancel h) |> List.filter isActive
        let todo = todo |> List.rev |> cancelIVRs |> List.rev
        let active = active |> cancelIVRs
        (active, todo)

    /// A generic algorithm for parallel running IVRs.
    /// The field. 
    /// Basically, all ivrs are processed in parallel, and as soon an ivr is
    /// completed, the arbiter is asked what to do.
    /// Note that the arbiter does not get to be asked again, as soon it cancels all the other ivrs and 
    /// sets a result.

    [<NoComparison;NoEquality>]
    type ArbiterDecision<'r> = 
        /// Cancel all remaining IVRs and set the result to the Arbiter's state
        | CancelField
        | AddToField of 'r ivr list

    type Arbiter<'state, 'r> = 'state -> Result<'r> -> ('state * ArbiterDecision<'r>)

    let field (arbiter: Arbiter<'state, 'r>) (initial: 'state) (ivrs: 'r ivr list) : 'state ivr = 
        
        let rec stepAll h stepF ((cancelling, state) as s) active ivrs =
            match ivrs with
            | [] -> s, active |> List.rev
            | ivr::todo ->
            let ivr = ivr |> stepF h
            match ivr with
            | Inactive _ -> failwith "internal error"
            | Active _ -> stepAll h stepF s (ivr::active) todo
            | Completed r ->
            if cancelling then
                // already cancelling, so we can ignore this result.
                stepAll h stepF s active todo
            else
            // ask the arbiter what to do next
            let state, decision = arbiter state r
            match decision with
            | CancelField -> (true, state), parCancel h active todo
            | AddToField newIVRs ->
            // start all new IVRs before putting them on the field.
            let (cancelling, state), newIVRs = newIVRs |> stepAll h start (false, state) []
            if cancelling then
                let (active, todo) = parCancel' h active todo
                (true, state), ([active |> List.rev; newIVRs; todo] |> List.collect id)
            else
            // embed by prepending them to the list of already processed IVRs
            // (don't use the current step function on the new ones)
            let active = (newIVRs |> List.rev) @ active
            stepAll h stepF (false, state) active todo
           
        and next ((_, state) as s, ivrs) = 
            match ivrs with
            | [] -> state |> Value |> Completed
            | _ -> active ivrs s |> Active

        and active ivrs s e h = 
            ivrs 
            |> stepAll h (fun h -> step h e) s []
            |> next
    
        fun (h: Host) ->
            ivrs
            |> stepAll h start (false, initial) []
            |> next
        |> Inactive

    /// Combine a list of ivrs so that they run in parallel. The resulting ivr ends when 
    /// All ivrs ended. When an error occurs in one of the ivrs, the resulting ivr ends with
    /// that error and all other ivrs are cancelled.

    let lpar (ivrs: 'r ivr list) : 'r list ivr = 
        
        let rec stepAll h stepF error active ivrs =
            match ivrs with
            | [] -> error, active |> List.rev
            | ivr::todo ->
            let ivr = ivr |> stepF h
            match ivr with
            | Completed (Error e) ->
                match error with
                | Some _ ->
                    // already got an error, so we can just forget this one and continue
                    stepAll h stepF error active todo
                | None ->
                    // we do have an error from now on, so we can remove all inactive and
                    // completed ivrs from the list after cancelling
                    Some e, parCancel h active todo
            | Completed _ ->
                // consume Completed results if we do have an error.
                let active = 
                    match error with
                    | Some _ -> active
                    | None -> ivr::active
                stepAll h stepF error active todo
            | Inactive _ -> failwith "internal error"
            | Active _ -> stepAll h stepF error (ivr::active) todo
        
        let rec active ivrs error e h = 
            ivrs 
            |> stepAll h (fun h -> step h e) error []
            |> arbiter
           
        and arbiter (error, ivrs) = 
            match error, ivrs with
            | Some e, [] -> e |> Error |> Completed
            | None, ivrs when ivrs |> List.forall isCompleted ->
                ivrs 
                |> List.map resultValue
                |> Value |> Completed
            | _ -> active ivrs error |> Active
    
        fun (h: Host) ->
            ivrs
            |> stepAll h start None []
            |> arbiter
        |> Inactive
    
    /// Runs a list of ivrs in parallel and finish with the first one that completes.
    /// Note that it may take an arbitrary number of time until the result is finally returned,
    /// because ivrs may refuse to get cancelled.
    let lpar' (ivrs: 'r ivr list) : 'r ivr =

        // Note: when an ivr finishes, all the running ones are canceled in the reversed 
        // order they were originally specified in the list 
        // (independent of how many of them already received the current event)!
        
        let rec stepAll h stepF res active ivrs =
            match ivrs with
            | [] -> res, (active |> List.rev)
            | ivr::todo ->
            let ivr = ivr |> stepF h
            match ivr with
            | Completed r ->
                match res with
                | Some _ ->
                    // already got a result, so we just forget the result and continue
                    stepAll h stepF res active todo
                | None ->
                    Some r, parCancel h active todo
            | Active _ ->
                stepAll h stepF res (ivr::active) todo
            | Inactive _ -> failwith "internal error"

        let rec active ivrs res e h = 
            ivrs 
            |> stepAll h (fun h -> step h e) res []
            |> arbiter
           
        and arbiter (result, ivrs) = 
            match result, ivrs with
            | Some r, [] -> r |> Completed
            | None, [] -> failwith "IVR.lpar': internal error: no active ivrs and no result"
            | res, ivrs -> Active (active ivrs res)
    
        fun (h: Host) ->
            ivrs
            |> stepAll h start None []
            |> arbiter
        |> Inactive

    /// Runs two ivrs in parallel, the resulting ivr completes, when both ivrs are completed.
    /// Events are delivered first to ivr1, then to ivr2. When one of the ivrs terminates without a result 
    /// (cancellation or exception),
    /// the resulting ivr is ended immediately.

    /// Note that par retains the result of the completed ivr, which
    /// could lead to leaks in nested parallel ivrs of which the result
    /// is never processed.

    let par (ivr1 : 'r1 ivr) (ivr2 : 'r2 ivr) : ('r1 * 'r2) ivr =

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
    let par' (ivr1 : 'r1 ivr) (ivr2 : 'r2 ivr) : Choice<'r1, 'r2> ivr =

        let ivr1 = ivr1 |> map Choice<_,_>.Choice1Of2
        let ivr2 = ivr2 |> map Choice<_,_>.Choice2Of2
        lpar' [ivr1; ivr2]

    /// Runs three ivrs in parallel, the resulting ivr completes with the result of the one that finishes first.
    let par'' (ivr1 : 'r1 ivr) (ivr2 : 'r2 ivr) (ivr3: 'r3 ivr) : Choice<'r1, 'r2, 'r3> ivr =

        let ivr1 = ivr1 |> map Choice<_,_,_>.Choice1Of3
        let ivr2 = ivr2 |> map Choice<_,_,_>.Choice2Of3
        let ivr3 = ivr3 |> map Choice<_,_,_>.Choice3Of3
        lpar' [ivr1; ivr2; ivr3]

    //
    // Sending and posting commands to the host.
    //

    /// Response type interface that is used to tag commands with.
    type IReturns<'result> = 
        interface end

    /// An IVR that synchronously sends a command to a host and returns its response.
    let send (cmd: IReturns<'r>) : 'r ivr = 
        fun (h: Host) ->
            try
                cmd
                |> h
                |> unbox 
                |> Value |> Completed
            with e ->
                e |> Error |> Completed
        |> Inactive

    /// An IVR that synchronously sends a command to a host, but ignores its response.
    let post cmd : unit ivr = 
        fun (h:Host) ->
            try
                cmd |> h |> Operators.ignore
                () |> Value |> Completed
            with e ->
                e |> Error |> Completed
        |> Inactive

    //
    // Simple computation expression to build sequential IVR processes
    //

    type IVRBuilder<'result>() = 
        member this.Bind(ivr: 'r ivr, body: 'r -> 'r2 ivr) : 'r2 ivr = 
            ivr
            |> continueWith (
                function 
                | Value r -> body r
                | Error err -> 
                    // kinda sad that we need to delay here.
                    fun _ -> err |> Error |> Completed
                    |> Inactive)

        member this.Return(v: 'r) : 'r ivr = 
            // tbd: this is probably delayed anyway, so we could return an
            // active ivr here (but then start must handle ivrs with a result)
            fun _ -> v |> Value |> Completed
            |> Inactive

        member this.ReturnFrom(ivr : 'r ivr) = ivr

        member this.Delay(f : unit -> 'r ivr) : 'r ivr = 
            fun h ->
                f()
                |> start h
            |> Inactive
              
        member this.Zero () : unit ivr = 
            fun _ -> () |> Value |> Completed
            |> Inactive

        member this.Using(disposable : 't, body : 't -> 'u ivr when 't :> IDisposable) : 'u ivr = 
            body disposable
            |> whenCompleted disposable.Dispose

        member this.TryFinally(ivr: 'r ivr, f: unit -> unit ivr) : 'r ivr =
            let finallyBlock res =

                let afterFinally finallyResult =
                    fun _ ->
                        match finallyResult with
                        | Error e -> e |> Error |> Completed
                        | _ -> res |> Completed
                    |> Inactive

                // note: f is already delayed, so we can run it in place.
                f() 
                |> continueWith afterFinally

            ivr
            |> continueWith finallyBlock

        member this.TryFinally(ivr: 'r ivr, f: unit -> unit) : 'r ivr =
            ivr
            |> whenCompleted f

        member this.TryWith(ivr: 'r ivr, eh: exn -> 'r ivr) : 'r ivr =
            ivr
            |> continueWith (function
                | Error e -> eh e
                | r -> (fun _ -> r |> Completed) |> Inactive)

        member this.Yield(cmd: Command) : unit ivr = post cmd

        member this.Combine(ivr1: unit ivr, ivr2: 'r ivr) : 'r ivr =
            ivr1
            |> continueWith (function
                | Error e -> (fun _ -> e |> Error |> Completed) |> Inactive
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
        let rec waiter (e: Event) _ =  
            match e with
            | :? Cancel -> Cancelled |> Error |> Completed
            | _ ->
            match f e with
            | Some r -> r |> Value |> Completed
            | None -> Active waiter

        fun _ ->
            Active waiter
        |> Inactive

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

    type DelayCompleted = DelayCompleted of Id

    let delay (ts: TimeSpan) =
        ivr {
            let! id = Delay ts |> send
            do! waitFor' (fun (DelayCompleted id') -> id' = id)
        }

    //
    // Async interopability
    //

    let fromResult(result: Result<'r>) = 
        fun _ ->
            result |> Completed
        |> Inactive

    // As long we don't support asynchronous runtimes, async computations are scheduled on
    // the threadpool by default.

    type IAsyncComputation = 
        abstract member run : (Result<obj> -> unit) -> unit

    type AsyncComputation<'r>(computation: Async<'r>) = 
        interface IAsyncComputation with
            /// Run the asynchronous computation on a threadpool thread and post 
            /// its result to the receiver.
            member this.run(receiver : Result<obj> -> unit) : unit = 
                async {
                    try
                        let! r = computation
                        r |> box |> Value |> receiver
                    with e ->
                        e |> Error |> receiver
                } |> Async.Start

        interface IReturns<Id>

    [<NoComparison>]
    type AsyncComputationCompleted = AsyncComputationCompleted of id: Id * result: Result<obj>

    /// Waits for an F# asynchronous computation.
    let await (computation: Async<'r>) : 'r ivr = 
        ivr {
            let! id = AsyncComputation(computation) |> send
            let! result = 
                waitFor(
                    fun (AsyncComputationCompleted (id', result)) -> 
                        if id' = id then Some result else None)
            return! fromResult (result.map unbox)
        }

#if false

    // Direct integration of async computations inside IVR computation expressions seems
    // to impair the type inferencer somewhat. So we use IVR.async for now.

    type IVRBuilder<'result> with
        member this.Bind(computation: Async<'r>, body: 'r -> 'r2 ivr) : 'r2 ivr = 
            let ivr = async computation
            this.Bind(ivr, body)

#endif

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

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
type 'result result =
    | Value of 'result
    | Error of exn

module Result =
    let map f r =
        match r with
        | Error e -> Error e
        | Value r -> Value (f r)

[<NoComparison;NoEquality>]
type 'result ivr = 
    | Inactive of (Host -> 'result ivr)
    | Active of (Event -> Host -> 'result ivr)
    | Completed of 'result result

module TimeSpanExtensions =

    type Int32 with
        member this.seconds = TimeSpan.FromSeconds(float this)
        member this.milliseconds = TimeSpan.FromMilliseconds(float this)

    type Double with
        member this.seconds = TimeSpan.FromSeconds(this)
        member this.milliseconds = TimeSpan.FromMilliseconds(this)

module internal List =
    let inline flatten l = List.collect id l
    let rec revAndPrepend a l = 
        match a with
        | next :: rest -> revAndPrepend rest (next::l) 
        | [] -> l        

[<RequireQualifiedAccess>]
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
    let continueWith (f : 'a result -> 'b ivr) (ivr: 'a ivr) : 'b ivr =

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
        let f (r: 'a result) = 
            fun _ -> r |> Result.map f |> Completed
            |> Inactive
        continueWith f ivr
    
    /// Lifts a value. Creates an IVR that returns the value.
    let ofValue (v: 'v) : 'v ivr = 
        fun _ -> v |> Value |> Completed
        |> Inactive

    /// Ignores the ivr's result type.
    let ignore ivr = ivr |> map ignore
            
    /// Invokes a function when the ivr is completed.
    let whenCompleted f =
        continueWith (fun r -> f(); (fun _ -> r |> Completed) |> Inactive)

    /// The result of a completed ivr.
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

    /// Maps a list of IVR's by applying the function f to each result.
    let internal lmap (f: 'a -> 'b) (ivrs: 'a ivr list) : ('b ivr list) = 
        let lf = map f
        ivrs |> List.map lf

    let internal lmapi (f: int -> 'a -> 'b) (ivrs: 'a ivr list) : ('b ivr list) = 
        ivrs 
        |> List.mapi (fun i -> f i |> map)

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

    //
    // field
    //

    [<NoComparison;NoEquality>]
    type ArbiterDecision<'state, 'r> = 
        /// Cancel all remaining IVRs and set the result of the field ivr
        | CancelField of 'state result
        /// Continue the field with a new state and optionally add some new players / IVRs to it.
        /// Note: when the field does not contain any more active ivrs, the 'state is returned
        /// as a final result of the field ivr.
        | ContinueField of 'state * 'r ivr list

    let cancelField r = CancelField r
    let continueField state ivrs = ContinueField(state, ivrs)

    type Arbiter<'state, 'r> = 'state -> 'r result -> (ArbiterDecision<'state, 'r>)

    /// A generic algorithm for running IVRs in parallel.
    /// The field:
    /// All ivrs are processed in parallel, and as soon an ivr completes, the arbiter is asked what to do.
    /// The arbiter can either decide to cancel the field and set a final result or
    /// continue the field with a 
    ///   an intermediate state/result
    ///   and a number of new ivrs that put on the field.
    /// Notes:
    ///   The arbiter does not get to be asked again, as soon it cancels the field.
    ///   When the arbiter throws an exception, it's equivalent to canceling the field with 
    ///   that exception as an error result.

    let field (arbiter: Arbiter<'state, 'r>) (initial: 'state) (ivrs: 'r ivr list) : 'state ivr = 

        // we need to protect the arbiter and handle its death
        let protectedArbiter state r = 
            let state = 
                state 
                |> function 
                | Value v -> v 
                | Error e -> failwithf "internal error: %A" e
            try
                arbiter state r
            with e ->
                CancelField (e |> Error)

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
            let decision = protectedArbiter state r
            match decision with
            | CancelField state -> (true, state), parCancel h active todo
            | ContinueField (state, newIVRs) ->
            // start all new IVRs before putting them on the field.
            let (cancelling, state), newIVRs = newIVRs |> stepAll h start (false, state |> Value) []
            if cancelling then
                let active, todo = parCancel' h active todo
                (true, state), [active |> List.rev; newIVRs; todo] |> List.flatten
            else
            // embed by prepending them to the list of already processed IVRs
            // (don't use the current step function on the new ones)
            let active = active |> List.revAndPrepend newIVRs
            stepAll h stepF (false, state) active todo
           
        and next ((_, state) as s, ivrs) = 
            match ivrs with
            | [] -> state |> Completed
            | _ -> active ivrs s |> Active

        and active ivrs s e h = 
            ivrs 
            |> stepAll h (fun h -> step h e) s []
            |> next
    
        fun (h: Host) ->
            ivrs
            |> stepAll h start (false, initial |> Value) []
            |> next
        |> Inactive
    
    /// game is a simpler version of the field, in which errors automatically lead to
    /// the cancellation of the game so that the game master does not need to handle them.

    let game master (initial: 'state) (ivrs: 'r ivr list) : 'state ivr = 
        let arbiter state (r: 'r result) = 
            match r with
            | Error e -> cancelField (Error e)
            | Value v -> master state v

        field arbiter initial ivrs

    /// Combine a list of ivrs so that they run in parallel. The resulting ivr ends when 
    /// All ivrs ended. When an error occurs in one of the ivrs, the resulting ivr ends with
    /// that error and all other ivrs are cancelled.

    let all (ivrs: 'r ivr list) : 'r list ivr = 

        // all can be implemented in terms of the field algorithm:

        // first attach indices
        let ivrs = ivrs |> lmapi (fun i r -> i, r)
        
        // arbiter collects the results in a map
        let arbiter state (r: (int * 'r) result) = 
            match r with
            | Value (i, r) -> 
                let state = state |> Map.add i r
                continueField state []
            | Error r -> cancelField (Error r)

        // and last convert the map to a list.
        let mapToList m = 
            m 
            |> Map.toList
            |> List.map snd

        ivrs 
        |> field arbiter Map.empty
        |> map mapToList
    
    /// Runs a list of ivrs in parallel and finish with the first one that completes.
    /// Note that it may take an arbitrary number of time until the result is finally returned,
    /// because ivrs may refuse to get cancelled.
    let any (ivrs: 'r ivr list) : 'r ivr =

        // Note: when an ivr finishes, all the running ones are canceled in the reversed 
        // order they were originally specified in the list 
        // (independent of how many of them already received the current event)!

        let arbiter _ = cancelField
        field arbiter Unchecked.defaultof<'r> ivrs

    /// Runs two ivrs in parallel, the resulting ivr completes, when both ivrs are completed.
    /// Events are delivered first to ivr1, then to ivr2. When one of the ivrs terminates without a result 
    /// (cancellation or exception),
    /// the resulting ivr is ended immediately.

    /// Note that join retains the result of the first completed ivr, which
    /// could lead to leaks in nested parallel ivrs of which the result
    /// is never processed.

    let join (ivr1 : 'r1 ivr) (ivr2 : 'r2 ivr) : ('r1 * 'r2) ivr =

        // we implement join in terms of all

        [map box ivr1; map box ivr2] 
        |> all
        |> map (function 
            | [l;r] -> unbox l, unbox r 
            | _ -> failwith "internal error: here to keep the compiler happy")

    /// Runs two ivrs in parallel, the resulting ivr completes with the result of the one that finishes first.
    /// events are delivered to ivr1 and then to ivr2, so ivr1 has an advantage when both complete in response to
    /// the same event. Note that if ivr1 completes, no event is delivered to ivr2.
    let first (ivr1 : 'r1 ivr) (ivr2 : 'r2 ivr) : Choice<'r1, 'r2> ivr =

        let ivr1 = ivr1 |> map Choice<_,_>.Choice1Of2
        let ivr2 = ivr2 |> map Choice<_,_>.Choice2Of2
        any [ivr1; ivr2]

    /// Runs three ivrs in parallel, the resulting ivr completes with the result of the one that finishes first.
    let first' (ivr1 : 'r1 ivr) (ivr2 : 'r2 ivr) (ivr3: 'r3 ivr) : Choice<'r1, 'r2, 'r3> ivr =

        let ivr1 = ivr1 |> map Choice<_,_,_>.Choice1Of3
        let ivr2 = ivr2 |> map Choice<_,_,_>.Choice2Of3
        let ivr3 = ivr3 |> map Choice<_,_,_>.Choice3Of3
        any [ivr1; ivr2; ivr3]

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
    // IDisposable, IVR style
    //
    // I am also introducing a new terminology here, the proc. A more generic term for
    // IVR. proc = small process.

    type IDisposableProc =
        inherit IDisposable
        abstract member DisposableProc : unit ivr

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
            ofValue v

        member this.ReturnFrom(ivr : 'r ivr) = ivr

        member this.Delay(f : unit -> 'r ivr) : 'r ivr = 
            fun h ->
                f()
                |> start h
            |> Inactive
              
        member this.Zero () : unit ivr = 
            fun _ -> () |> Value |> Completed
            |> Inactive

        member this.Using(disposable : 't, body : 't -> 'r ivr when 't :> IDisposable) : 'r ivr =
            let body = body disposable
            match box disposable with
            | :? IDisposableProc as dp -> this.TryFinally(body, fun () -> dp.DisposableProc)
            | _ -> this.TryFinally(body, disposable.Dispose)

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

    /// Construct an IDisposableProc from a unit ivr, so that this ivr can be used
    /// with F# 'use' keyword inside a computation expression.
    let asDisposable (ivr: unit ivr) =
        { 
            new IDisposableProc with
                member __.Dispose() = ()
                member x.DisposableProc = ivr
        }
            
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

    /// Waits for some event by asking a predicate for each event that comes along.
    /// Continues waiting when the predicate returns false, ends the ivr when the predicate 
    /// returns true.
    let wait' predicate = 
        let f e = 
            let r = predicate e
            match r with 
            | true -> Some ()
            | false -> None

        wait f

    /// Waits for an event of a type derived by the function f that is passed in. 
    /// Ends the ivr with the value returned by the function if it returns (Some value), 
    /// continues waiting when f returns None
    let waitFor (f : 'e -> 'r option) = 

        let f (e : Event) = 
            match e with
            | :? 'e as e -> f e
            | _ -> None

        wait f

    /// Waits for an event of a type derived by the function f that is passed in.
    /// Ends the ivr when f return true. Continues waiting when f returns false.
    let waitFor' (f : 'e -> bool) =
        let f (e: Event) =
            match e with
            | :? 'e as e -> f e
            | _ -> false

        wait' f

    /// Waits forever.
    let idle = wait' (fun _ -> false)

    //
    // IVR System Commands & Events
    //

    type Delay = Delay of TimeSpan
        with
        interface IReturns<Id>

    type DelayCompleted = DelayCompleted of Id

    /// Wait for the given time span and continue then.
    let delay (ts: TimeSpan) =
        ivr {
            let! id = Delay ts |> send
            do! waitFor' (fun (DelayCompleted id') -> id' = id)
        }

    /// Deliver an event to the currently active processes.
    [<NoComparison>]
    type Schedule = Schedule of Event
        with
        member this.event = let (Schedule e) = this in e
    
    let schedule (e: Event) = 
        e |> Schedule |> post

    //
    // Async interopability
    //

    let fromResult(result: 'r result) = 
        fun _ ->
            result |> Completed
        |> Inactive

    // As long we don't support asynchronous runtimes, async computations are scheduled on
    // the threadpool by default.

    type IAsyncComputation = 
        abstract member run : (obj result -> unit) -> unit

    type AsyncComputation<'r>(computation: Async<'r>) = 
        interface IAsyncComputation with
            /// Run the asynchronous computation on a threadpool thread and post 
            /// its result to the receiver.
            member this.run(receiver : obj result -> unit) : unit = 
                async {
                    try
                        let! r = computation
                        r |> box |> Value |> receiver
                    with e ->
                        e |> Error |> receiver
                } |> Async.Start

        interface IReturns<Id>

    [<NoComparison>]
    type AsyncComputationCompleted = AsyncComputationCompleted of id: Id * result: obj result

    /// Waits for an F# asynchronous computation.
    let await (computation: Async<'r>) : 'r ivr = 
        ivr {
            let! id = AsyncComputation(computation) |> send
            let! result = 
                waitFor(
                    fun (AsyncComputationCompleted (id', result)) -> 
                        if id' = id then Some result else None)
            return! fromResult (result |> Result.map unbox)
        }

/// Helpers for mapping values of IVR lists
module IVRs = 
    let map f ivrs = IVR.lmap
    let mapi f ivrs = IVR.lmapi


#if false

    // Direct integration of async computations inside IVR computation expressions seems
    // to impair the type inferencer somewhat. So we use IVR.async for now.

    type IVRBuilder<'result> with
        member this.Bind(computation: Async<'r>, body: 'r -> 'r2 ivr) : 'r2 ivr = 
            let ivr = async computation
            this.Bind(ivr, body)

#endif

module BuilderExtensions = 
    let ivr<'r> = IVR.ivr<'r>

// a little inception may not do any harm :)

[<assembly:AutoOpen("IVR.TimeSpanExtensions")>]
[<assembly:AutoOpen("IVR.BuilderExtensions")>]
do ()

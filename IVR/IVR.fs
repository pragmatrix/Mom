namespace IVR

open System

(*
    An IVR is a definition of an asynchronous process with the following properties:
    
    - can be paused and ended at any time.
    - can synchronously wait and respond to events.
    - can be combined in parallel or sequentially.
    - can send any number of commands to a host.
    - can be run in steps.
*)

type Event = obj
type Command = obj
type Response = obj
type Host = Command -> Response

exception CancelledException

[<NoComparison>]
type 'result result =
    | Value of 'result
    | Error of exn
    | Cancelled

module Result =
    let map f r =
        match r with
        | Value r -> Value (f r)
        | Error e -> Error e
        | Cancelled -> Cancelled

[<NoComparison;NoEquality>]
type 'result ivr = 
    | Inactive of (Host -> 'result ivr)
    | Active of (Event -> 'result ivr)
    | Completed of 'result result

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
    let rec step e ivr = 
        match ivr with
        // may be we should start it here, too?
        //> no: delays are not supported, once an IVR starts, subsequential
        //> IVRs do have to be started before stepping through

        | Active f -> 
            try
                f e
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

    /// Returns the error of a completed ivr.
    let error ivr = 
        match ivr with
        | Completed (Error e) -> e
        | _ -> failwithf "IVR.error: ivr is not in error: %A" ivr

    let isCancelled ivr = 
        match ivr with
        | Completed Cancelled -> true
        | _ -> false

    //
    // Primitives Part 2
    //

    /// Continues the ivr with a followup ivr (Monad bind).
    let continueWith (f : 'a result -> 'b ivr) (ivr: 'a ivr) : 'b ivr =

        let rec next h state = 
            match state with
            | Active _ -> 
                fun e -> state |> step e |> next h
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
    
    /// Lifts a result.
    let ofResult (r: 'r result) : 'r ivr = 
        fun _ -> r |> Completed
        |> Inactive

    /// Lifts a value. Creates an IVR that returns the value.
    let ofValue (v: 'v) : 'v ivr = 
        v |> Value |> ofResult
    
    /// Lifts an error.
    let ofError e : 'v ivr =
        e |> Error |> ofResult

    /// Ignores the ivr's result type.
    let ignore ivr = ivr |> map ignore
            
    /// Invokes a function when the ivr is completed.
    let whenCompleted f =
        continueWith (fun r -> f(); r |> ofResult)

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

    /// The event that is sent to an active IVR when it gets cancelled. Note that this is basically a hint to 
    /// the IVR to begin cancellation and does not need to be respected and also it may take an arbitrary 
    /// amount of time until the IVR finally completes.
    type TryCancel = TryCancel

    /// Tries to cancels the ivr. This actually sends a Cancel event to the ivr, but otherwise
    /// does nothing. The ivr is responsible to react on Cancel events (for example every wait function does that).
    /// Also note that ivrs can continue after cancellation was successful. For example finally
    /// handlers can run ivrs.
    /// If an ivr is completed, the result is overwritten (freed indirectly) with a Cancelled error,
    /// but an error is not, to avoid shadowing the error.
    /// If an ivr is Inactive, it is also converted to Cancelled error, to prevent anyone else
    /// from activating it and to free the code behind the activation function.
    let tryCancel ivr = 
        match ivr with
        | Active _ -> 
            ivr |> step TryCancel
        | Completed (Error _) -> ivr
        | _ -> Cancelled |> Completed

    //
    // IVR Combinators
    //

    /// Maps a list of IVR's by applying the function f to each result.
    let internal lmap (f: 'a -> 'b) (ivrs: 'a ivr list) : ('b ivr list) = 
        let lf = map f
        ivrs |> List.map lf

    let internal lmapi (f: int -> 'a -> 'b) (ivrs: 'a ivr list) : ('b ivr list) = 
        ivrs 
        |> List.mapi (f >> map)

    // Cancels a pair of ivr lists in the reversed specified order while in the process of 
    // processing a step for all parallel ivrs.
    // active are the ones that where already processed (in the reversed order specified).
    // todo are the ones that have not yet processed for this round (in the specified order).
    // Returns the ivrs that stay active after the cancellation was sent.

    let private parCancel active todo = 
        let todo = todo |> List.rev
        todo @ active
        |> List.map tryCancel 
        |> List.filter isActive 
        |> List.rev         


    let private parCancel' active todo = 
        let cancelIVRs ivrs = ivrs |> List.map tryCancel |> List.filter isActive
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

    [<NoComparison;NoEquality>]
    type private FieldState<'state> = 
        | FieldActive of 'state
        | FieldCancelling of 'state result

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
            try
                arbiter state r
            with e ->
                CancelField (e |> Error)
    
        fun (h: Host) ->

            let rec stepAll stepF (state: FieldState<'state>) active ivrs =
                match ivrs with
                | [] -> state, active |> List.rev
                | ivr::todo ->
                let ivr = ivr |> stepF
                match ivr with
                | Inactive _ -> failwith "internal error"
                | Active _ -> stepAll stepF state (ivr::active) todo
                | Completed r ->
                match state with
                | FieldCancelling _ ->
                    // already cancelling, so we can ignore this result.
                    stepAll stepF state active todo
                | FieldActive s ->

                // ask the arbiter what to do next
                let decision = protectedArbiter s r
                match decision with
                | CancelField state -> FieldCancelling state, parCancel active todo
                | ContinueField (s, newIVRs) ->
                let state = FieldActive s
                // start all new IVRs before putting them on the field.
                let state, newIVRs = 
                    newIVRs |> stepAll (start h) state []
                match state with
                | FieldCancelling _ ->
                    let active, todo = parCancel' active todo
                    state, [active |> List.rev; newIVRs; todo] |> List.flatten
                | FieldActive _ ->
                // embed by prepending them to the list of already processed IVRs
                // (don't use the current step function on the new ones)
                let active = active |> List.revAndPrepend newIVRs
                stepAll stepF state active todo
           
            and next (state: FieldState<'state>, ivrs) = 
                match ivrs with
                | [] -> 
                    match state with
                    | FieldActive state -> state |> Value |> Completed
                    | FieldCancelling state -> state |> Completed
                | ivrs -> active ivrs state |> Active

            and active ivrs s e = 
                ivrs 
                |> stepAll (step e) s []
                |> next

            ivrs
            |> stepAll (start h) (FieldActive initial) []
            |> next
        |> Inactive
    
    /// game is a simpler version of the field, in which errors automatically lead to
    /// the cancellation of the game so that the game master does not need to handle them.

    let game master (initial: 'state) (ivrs: 'r ivr list) : 'state ivr = 
        let arbiter state (r: 'r result) = 
            match r with
            | Value v -> master state v
            | Error e -> cancelField (Error e)
            | Cancelled -> cancelField Cancelled

        field arbiter initial ivrs

    /// Combine a list of ivrs so that they run in parallel. The resulting ivr ends when 
    /// all ivrs ended. When an error occurs in one of the ivrs, the resulting ivr ends with
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
            | Cancelled -> cancelField Cancelled

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
    // This is also introducing a new terminology here that should replace 'ivr' in the future,
    // the proc. proc = small process.

    type IDisposableProc =
        inherit IDisposable
        abstract member DisposableProc : unit ivr

    //
    // Cancellation Helper
    //

    /// Exception / Error that represents a nested cancellation error

    exception NestedCancellationException

    /// Install an cancallation ivr for the ivr body. That cancellation ivr is called 
    /// when the code inside the block gets cancelled. This function can only be used in a use! 
    /// instruction inside of a computation expression.

    let cancelWith cancelIVR body = 
        let afterBody rBody =
            let afterCancel rCancellation = 
                match rCancellation with
                | Value _ -> rBody |> ofResult
                | Error e -> e |> ofError
                // the cancellation ivr got cancelled! This is an error for now!
                // tbd: An cancellation ivr must be protected from further cancellation.
                | Cancelled -> NestedCancellationException |> ofError

            match rBody with
            | Cancelled -> cancelIVR |> continueWith afterCancel
            | _ -> rBody |> ofResult

        body
        |> continueWith afterBody

    //
    // Simple computation expression to build sequential IVR processes
    //

    type IVRBuilder<'result>() = 
        
        let delay (f : unit -> 'r ivr) : 'r ivr = 
            fun h -> f() |> start h
            |> Inactive

        member __.Bind(ivr: 'r ivr, body: 'r -> 'r2 ivr) : 'r2 ivr = 
            ivr
            |> continueWith (function 
                | Value r -> body r
                | Error err -> err |> ofError
                | Cancelled -> Cancelled |> ofResult)

        // tbd: this is probably delayed anyway, so we could return an
        // active ivr here (but then start must handle ivrs with a result)
        member __.Return(v: 'r) : 'r ivr = ofValue v
        member __.ReturnFrom(ivr : 'r ivr) = ivr
        member __.Delay(f : unit -> 'r ivr) : 'r ivr = delay f
        member __.Zero () : unit ivr = ofValue ()

        member this.Using(disposable : 't, body : 't -> 'r ivr when 't :> IDisposable) : 'r ivr =
            let body = body disposable
            match box disposable with
            | :? IDisposableProc as dp -> 
                this.TryFinally(body, fun () -> dp.DisposableProc)
            | _ -> 
                this.TryFinally(body, disposable.Dispose)

        member __.TryFinally(ivr: 'r ivr, f: unit -> unit ivr) : 'r ivr =
            let finallyBlock tryResult =

                // if the finally ivr results in an error or cancellation, 
                // this is our result, otherwise return the result.
                let afterFinally finallyResult =
                    match finallyResult with
                    | Value _ -> tryResult |> ofResult
                    | Error e -> e |> ofError
                    // if the finallly {} got cancelled, our whole
                    // block's result is Cancelled
                    | Cancelled -> Cancelled |> ofResult

                // note: f is already delayed, so we can run it in place.
                f() 
                |> continueWith afterFinally

            ivr
            |> continueWith finallyBlock

        member __.TryFinally(ivr: 'r ivr, f: unit -> unit) : 'r ivr =
            ivr
            |> whenCompleted f

        member __.TryWith(ivr: 'r ivr, eh: exn -> 'r ivr) : 'r ivr =
            ivr
            |> continueWith (function
                | Error e -> eh e
                | r -> r |> ofResult)
                
        member __.Combine(ivr1: unit ivr, ivr2: 'r ivr) : 'r ivr =
            ivr1
            |> continueWith (function
                | Error e -> e |> ofError
                | Cancelled -> Cancelled |> ofResult
                | Value _ -> ivr2
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
                member __.DisposableProc = ivr
        }
            
    //
    // Wait primitives
    //

    /// An IVR that waits for some event given a function that returns (Some result) or None.
    let wait f =
        let rec waiter (e: Event) =  
            match e with
            | :? TryCancel -> Cancelled |> Completed
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

    type Delay = 
        | Delay of TimeSpan
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
    type Schedule = 
        | Schedule of Event
        member this.Event = let (Schedule e) = this in e
    
    let schedule (e: Event) = 
        e |> Schedule |> post

    //
    // Async interopability
    //

    // async computations are scheduled on the threadpool by default.

    type IAsyncComputation = 
        abstract member Run : (obj result -> unit) -> unit

    type AsyncComputation<'r>(computation: Async<'r>) = 
        interface IAsyncComputation with
            /// Run the asynchronous computation on a threadpool thread and post 
            /// its result to the receiver.
            member __.Run(receiver : obj result -> unit) : unit = 
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
            return! (result |> Result.map unbox) |> ofResult
        }

/// Helpers for mapping values of IVR lists
module IVRs = 
    let map f ivrs = IVR.lmap f ivrs
    let mapi f ivrs = IVR.lmapi f ivrs

module BuilderExtensions = 
    let ivr<'r> = IVR.ivr<'r>

// a little inception may not do any harm :)
[<assembly:AutoOpen("IVR.BuilderExtensions")>]
do ()

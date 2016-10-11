namespace IVR

open System

(*
    An IVR is a definition of an asynchronous process with the following properties:
    
    - can be paused and ended at any time.
    - can synchronously wait and respond to events.
    - can be combined in parallel or sequentially.
    - can send any number of requests to a host.
    - can be run in steps.
*)

/// None means that the IVR is waiting for an event only.
type Request = obj
type Response = obj
type Event = obj
type Host = Request -> Response

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
    | Delayed of (unit -> 'result ivr)
    | Active of Request option * (Response -> 'result ivr)
    | Completed of 'result result

[<RequireQualifiedAccess>]
module IVR = 

    // 
    // IVR Primitives Part 1
    //

    /// Protect a function that creates an ivr by returning an ivr error in case of an exception.
    let private protect f = 
        fun response ->
            try
                f response
            with e ->
                e |> Error |> Completed

    /// Start up an ivr.
    let start ivr = 
        match ivr with
        | Delayed f -> protect f ()
        | ivr -> failwithf "IVR.start: can't start an ivr that is not inactive: %A" ivr

    /// Dispatch an event to the ivr that is currently waiting for one.
    let dispatch ev ivr =
        match ivr with
        | Active(None, cont) -> cont ev
        | ivr -> failwithf "IVR.dispatch: can't dispatch an event to an ivr that is not waiting for one: %A" ivr
        
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
    let continueWith (followup : 'a result -> 'b ivr) (ivr: 'a ivr) : 'b ivr =

        let rec next state = 
            match state with
            | Active (req, cont) ->
                Active (req, cont >> (protect next))
            | Completed r -> 
                followup r |> start
            | Delayed _ ->
                failwithf "IVR.continueWith, ivr is delayed: %A" state

        Delayed <|
        fun () ->
            ivr |> start |> next
    
    /// Maps the ivr's result. In other words: lifts a function that converts a value from a to b
    /// into the IVR category.
    let map (f: 'a -> 'b) (ivr: 'a ivr) : 'b ivr = 
        let f (r: 'a result) = 
            fun _ -> r |> Result.map f |> Completed
            |> Delayed
        continueWith f ivr
    
    /// Lifts a result.
    let ofResult (r: 'r result) : 'r ivr = 
        fun _ -> r |> Completed
        |> Delayed

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
    /// An ivr that is delayed (not started) or waiting for a synchronous response, can not be cancelled.
    let tryCancel ivr = 
        match ivr with
        | Active (None, cont) -> 
            cont TryCancel
        | Active (Some _, _) ->
            failwith "failed to cancel an active IVR that is waiting for a synchronous response"
        | Completed (Error _) -> ivr
        | Completed _ -> Cancelled |> Completed
        | Delayed _ ->
            failwith "failed to cancel an IVR that has not been started yet"

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

    [<NoEquality; NoComparison>]
    type private Field<'state, 'r> = {
        State: 'state
        Event: Response option
        Pending: 'r ivr list
        /// All ivrs that got the event already or new ivrs, all in reversed order.
        Processed: 'r ivr list
    }

    /// A generic algorithm for running IVRs in parallel.
    /// The field:
    /// All ivrs are processed in parallel, and as soon an ivr completes, the arbiter is asked what to do.
    /// The arbiter can either decide to cancel the field and set a final result or
    /// continue the field with a 
    ///   an intermediate state/result
    ///   and a number of new ivrs that put on the field.
    /// Notes:
    ///   The arbiter does not get to be asked again, as soon it cancels the field.
    ///   When the arbiter throws an exception, it's equivalent to cancelling the field with 
    ///   that exception as an error result.
    ///   Cancellation is processed in reversed field insertion order.
    
    let field (arbiter: Arbiter<'state, 'r>) (initial: 'state) (ivrs: 'r ivr list) : 'state ivr = 

        let rev = List.rev

        // if the arbiter crashes, the field will be cancelled.
        let arbiter state r = 
            try
                arbiter state r
            with e ->
                CancelField (e |> Error)

        /// As long new IVRs need to be added to the field we try to bring them into a state when they
        /// want to receive events only.
        let rec enter (field: Field<'state, 'r>) pending =
            match pending with
            | [] -> proceed field
            | ivr :: pending ->
            match ivr with
            | Delayed _ -> enter field ((start ivr) :: pending)
            | Active (Some _ as request, cont) ->
                Active(request, cont >> fun ivr -> enter field (ivr::pending))
            | Active (None, _) -> 
                // as long new ivrs are added to the field, event processing is delayed.
                enter { field with Processed = ivr :: field.Processed } pending
            | Completed result ->
            match arbiter field.State result with
            | ContinueField (newState, moreIVRs) ->
                enter { field with State = newState } (moreIVRs @ pending)
            | CancelField result ->
                cancel result [] ((rev field.Pending) @ field.Processed)
        
        // Move the field forward.
        and proceed (field: Field<'state, 'r>) =
            match field.Event with
            | None ->
                assert(field.Pending.IsEmpty)
                match field.Processed with
                | [] -> exit (field.State |> Value)
                | processed -> Active (None, fun ev -> proceed { field with Event = Some ev; Pending = rev processed; Processed = [] })
            | Some ev ->
                match field.Pending with
                | [] -> proceed { field with Event = None }
                | ivr::pending ->
                match ivr with
                | Delayed _
                | Completed _
                | Active (Some _, _) -> failwithf "internal error: %A in field pending" ivr
                | Active (None, cont) ->
                // deliver the event and be sure that the ivr is removed from pending
                cont ev 
                |> drain (postProcess { field with Pending = pending })

        // Continue processing the field or ask the arbiter what to do if the ivr is completed.
        and postProcess (field: Field<'state, 'r>) ivr =
            match ivr with
            | Delayed _
            | Active (Some _, _) -> 
                failwithf "internal error: %A in post processing" ivr
            | Active (None, _) -> proceed { field with Processed = ivr::field.Processed }
            | Completed result ->
            match arbiter field.State result with
            | ContinueField (newState, newIVRs) ->
                enter { field with State = newState } newIVRs
            | CancelField result ->
                cancel result [] ((rev field.Pending) @ field.Processed)

        // Cancellation:
        // send TryCancel as soon they are in waiting state, if not, process host requests until they are.
        and cancel result cancelled pending =
            match pending with
            | [] -> finalize result cancelled
            | ivr::pending ->
            match ivr with
            | Delayed _ -> failwithf "internal error: %A in field cancellation" ivr
            | Active (Some _ as request, cont) ->
                Active(request, cont >> fun ivr -> cancel result cancelled (ivr::pending))
            | Active (None, _) ->
                let ivr = tryCancel ivr
                cancel result (ivr::cancelled) pending
            | Completed _ ->
                cancel result cancelled pending

        // Finalization: run them until they are all gone.
        and finalize result pending =
            match pending with
            | [] -> exit result
            | ivr::pending ->
            match ivr with
            | Delayed _ -> failwithf "internal error: %A in field finalization" ivr
            | Active (request, cont) ->
                Active(request, cont >> fun ivr -> finalize result (ivr::pending))
            | Completed _ ->
                finalize result pending

        and exit result =
            result |> Completed               

        /// Process the ivr until it's completed or waiting for an event, and then call
        /// the continuation method.
        and drain followUp ivr = 
            match ivr with
            | Delayed _ -> failwithf "internal error: %A in field stall" ivr
            | Active (Some _ as request, cont) ->
                Active(request, cont >> drain followUp)
            | Active (None, _)
            | Completed _ ->
                followUp ivr

        Delayed <|
        fun () ->
            enter { State = initial; Event = None; Pending = []; Processed = [] } ivrs

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

        // and at last, convert the map to a list.
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

        // join is implemented in terms of all

        [map box ivr1; map box ivr2] 
        |> all
        |> map (function 
            | [l;r] -> unbox l, unbox r 
            | _ -> failwith "internal error")

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
    // Posting and sending requests to the host.
    //

    /// An IVR that synchronously sends a request to a host and ignores its response.
    let post request : unit ivr = 
        // tbd: do we need to delay here anymore?
        fun () ->
            Active(Some request, fun _ -> Value () |> Completed)
        |> Delayed

    /// Response type interface that is used to tag requests with that return a value. 
    /// Tag data types with this interface and use them as a request with IVR.send so that 
    /// IVR.send is able to cast the resulting value. Use IRequest<unit> for Requests that return
    /// no value.
    type IRequest<'response> = 
        interface end

    /// An IVR that synchronously sends a request to a host and returns its response. The requests
    /// need to implement the IRequest<_> interface so that the returned response value can be typed
    /// properly.
    let send (cmd: IRequest<'r>) : 'r ivr = 
        fun () ->
            Active(Some (box cmd), unbox >> Value >> Completed)
        |> Delayed

    //
    // IDisposable, Flow style
    //

    type IDisposableFlow =
        inherit IDisposable
        abstract member DisposableFlow : unit ivr

    //
    // Cancellation Helper
    //

    /// Exception / Error that represents a nested cancellation error

    exception NestedCancellationException

    /// Install an cancellation ivr for the ivr body. That cancellation ivr is called 
    /// when the code inside the block gets cancelled. This function can only be used in a use! 
    /// instruction inside of a computation expression.

    let onCancel cancelIVR body = 
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

        member __.Source(ivr: 'r ivr) : 'r ivr = ivr
        member __.Source(r: 'r when 'r :> IRequest<'rr>) = r |> send
        member __.Source(s: 'e seq) = s

        member __.Bind(ivr: 'r ivr, body: 'r -> 'r2 ivr) : 'r2 ivr = 
            ivr |> continueWith (function 
                | Value r -> body r
                | Error err -> err |> ofError
                | Cancelled -> Cancelled |> ofResult)

        // tbd: this is probably delayed anyway, so we could return an
        // active ivr here (but then start must handle ivrs with a result)
        member __.Return(v: 'r) : 'r ivr = ofValue v
        member __.ReturnFrom(ivr : 'r ivr) = ivr
        member this.Delay(f : unit -> 'r ivr) : 'r ivr = 
            this.Bind(this.Return(), f)
        member this.Zero () : unit ivr = 
            this.Return()

        member this.Using(disposable : 't, body : 't -> 'r ivr when 't :> IDisposable) : 'r ivr =
            let body = body disposable
            match box disposable with
            | :? IDisposableFlow as dp -> 
                this.TryFinally(body, fun () -> dp.DisposableFlow)
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
                f() |> continueWith afterFinally

            ivr |> continueWith finallyBlock

        member __.TryFinally(ivr: 'r ivr, f: unit -> unit) : 'r ivr =
            ivr |> whenCompleted f

        member __.TryWith(ivr: 'r ivr, eh: exn -> 'r ivr) : 'r ivr =
            ivr |> continueWith (function
                | Error e -> eh e
                | r -> r |> ofResult)
                
        member this.Combine(ivr1: unit ivr, ivr2: 'r ivr) : 'r ivr =
            this.Bind(ivr1, fun () -> ivr2)

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
    let asDisposable (ivr: unit ivr) = { 
        new IDisposableFlow with
            member __.Dispose() = ()
            member __.DisposableFlow = ivr
    }
            
    //
    // Wait primitives
    //

    /// An IVR that waits for some event given a function that returns (Some result) or None.
    let wait f =
        let rec waiter (ev: Event) =  
            match ev with
            | :? TryCancel -> Cancelled |> Completed
            | _ ->
            match f ev with
            | Some r -> r |> Value |> Completed
            | None -> Active (None, protect waiter)

        Delayed <|
        fun _ -> Active (None, protect waiter)

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

        let f (ev : Event) = 
            match ev with
            | :? 'e as e -> f e
            | _ -> None

        wait f

    /// Waits for an event of a type derived by the function f that is passed in.
    /// Ends the ivr when f return true. Continues waiting when f returns false.
    let waitFor' (f : 'e -> bool) =
        let f (ev: Event) =
            match ev with
            | :? 'e as e -> f e
            | _ -> false

        wait' f

    /// Waits forever.
    let idle = wait' (fun _ -> false)

    //
    // IVR System Requests & Events
    //

    type Delay = 
        | Delay of TimeSpan
        interface IRequest<Id>

    type DelayCompleted = DelayCompleted of Id

    /// Wait for the given time span and continue then.
    let delay (ts: TimeSpan) =
        ivr {
            if ts < TimeSpan.Zero then
                failwithf "IVR.delay: unsupported negative time span: %s" (ts |> string)
            if ts <> TimeSpan.Zero then
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
    // IVR System combinators
    //

    /// Process the ivr given and return it's value as an option or
    /// timeout after the given timespan and return None.
    let timeoutAfter (ts: TimeSpan) ivr = 
        any [
            ivr |> map Some
            delay ts |> map (fun () -> None)
        ]

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

        interface IRequest<Id>

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

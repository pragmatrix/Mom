namespace IVR

open System
open IVR.Flux

(*
    An IVR is a definition of an asynchronous process with the following properties:
    
    - can be paused and ended at any time.
    - can synchronously wait and respond to events.
    - can be combined in parallel or sequentially.
    - can send any number of requests to a host.
    - can be run in steps.
*)

[<RequireQualifiedAccess>]
module IVR = 

    [<NoComparison;NoEquality>]
    type 'result ivr = unit -> 'result flux

    // 
    // IVR Primitives Part 1
    //

    /// Start up an ivr.
    let start (ivr : _ ivr) = ivr ()

    //
    // Primitives Part 2
    //
    
    /// Lifts a result.
    let ofResult (r: 'r result) : 'r ivr = 
        fun () -> r |> Completed

    /// Lifts a value. Creates an IVR that returns the value.
    let ofValue (v: 'v) : 'v ivr = 
        v |> Value |> ofResult
    
    /// Lifts an error.
    let ofError e : 'v ivr =
        e |> Error |> ofResult

    /// Continues the ivr with a followup ivr (Monad bind).
    let continueWith (followup : 'a result -> 'b ivr) (ivr: 'a ivr) : 'b ivr =

        let rec next state = 
            match state with
            | Requesting (req, cont) ->
                Requesting (req, cont >> next)
            | Waiting cont ->
                Waiting (cont >> next)
            | Completed r -> 
                try followup r |> start
                with e -> e |> Error |> Completed

        fun () ->
            ivr |> start |> next

    let bind body ivr = 
        ivr |> continueWith (function 
            | Value r -> body r
            | Error err -> err |> ofError
            | Cancelled -> Cancelled |> ofResult)
    
    /// Maps the ivr's result. In other words: lifts a function that converts a value from a to b
    /// into the IVR category.
    let map (f: 'a -> 'b) (ivr: 'a ivr) : 'b ivr = 
        let f (r: 'a result) = 
            fun () -> r |> Result.map f |> Completed
        continueWith f ivr

    /// Ignores the ivr's result.
    let ignore ivr = ivr |> map ignore

    /// Runs the ivr, ignores its result, and then changes the ivr's return value to the value given. 
    let force value ivr = ivr |> map (fun _ -> value)
            
    /// Invokes a function when the ivr is completed.
    let whenCompleted f =
        continueWith (fun r -> f(); r |> ofResult)

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
        | CancelField of 'state result
        | ContinueField of 'state * 'r ivr list

    /// Cancel all remaining IVRs and set the result of the field ivr
    let cancelField r = CancelField r

    /// Continue the field with a new state and optionally add some new players / IVRs to it.
    /// Note: when the field does not contain any more active ivrs, the 'state is returned
    /// as a final result of the field ivr.
    let continueField state ivrs = ContinueField(state, ivrs)

    type Arbiter<'state, 'r> = 'state -> 'r result -> (ArbiterDecision<'state, 'r>)

    [<NoEquality; NoComparison>]
    type private Field<'state, 'r> = {
        State: 'state
        Event: Response option
        Pending: 'r flux list
        /// All ivrs that got the event already or new ivrs, all in reversed order.
        Processed: 'r flux list
    }

    /// A generic algorithm for running IVRs in parallel.
    /// The field:
    /// All ivrs are processed in parallel, and as soon an ivr completes, the arbiter is asked what to do.
    /// The arbiter can either decide to cancel the field and set a final result or
    /// continue the field with a 
    ///   an intermediate state/result
    ///   and a number of new ivrs to add to the field.
    /// Notes:
    ///   The arbiter does not get to be asked again, after it cancels the field.
    ///   When the arbiter throws an exception, it's equivalent to cancelling the field with 
    ///   that exception as an error result.
    ///   Cancellation is processed in reversed field insertion order.
    
    let field' (arbiter: Arbiter<'state, 'r>) (initial: 'state) (ivrs: 'r ivr list) : 'state ivr = 

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
            enter2 field (start ivr) pending
        
        and enter2 (field: Field<'state, 'r>) flux pending =
            match flux with
            | Requesting (request, cont) -> 
                Requesting (request, cont >> fun flux -> enter2 field flux pending)
            | Waiting _ -> 
                // as long new ivrs are added to the field, event processing is delayed.
                enter { field with Processed = flux::field.Processed } pending
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
                | [] -> 
                    exit (field.State |> Value)
                | processed -> 
                    Waiting (fun ev -> proceed { field with Event = Some ev; Pending = rev processed; Processed = [] })
            | Some ev ->
                match field.Pending with
                | [] -> proceed { field with Event = None }
                | flux::pending ->
                match flux with
                | Completed _
                | Requesting _ -> failwithf "internal error: %A in field pending" flux
                | Waiting cont ->
                // deliver the event and be sure that the ivr is removed from pending
                cont ev |> postProcess { field with Pending = pending }

        // Continue processing the field or ask the arbiter what to do if the ivr is completed.
        and postProcess (field: Field<'state, 'r>) flux =
            match flux with
            | Requesting (request, cont) -> 
                Requesting (request, cont >> fun flux -> postProcess field flux)
            | Waiting _ -> proceed { field with Processed = flux::field.Processed }
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
            | flux::pending ->
            match flux with
            | Requesting (request, cont) ->
                Requesting(request, cont >> fun flux -> cancel result cancelled (flux::pending))
            | Waiting _ ->
                cancel result ((tryCancel flux)::cancelled) pending
            | Completed _ ->
                cancel result cancelled pending

        // Finalization: run them until they are all gone.
        and finalize result pending =
            match pending with
            | [] -> exit result
            | flux::pending ->
            match flux with
            | Requesting (request, cont) ->
                Requesting (request, cont >> fun flux -> finalize result (flux::pending))
            | Waiting cont ->
                // actually, this is currently not supported. See issue #4
                Waiting (cont >> fun flux -> finalize result (flux::pending))
            | Completed _ ->
                finalize result pending

        and exit result =
            result |> Completed

        fun () ->
            enter { State = initial; Event = None; Pending = []; Processed = [] } ivrs

    /// field is a simpler version of the field, in which errors automatically lead to
    /// the cancellation of the field so that the arbiter does not need to handle them.

    let field arbiter (initial: 'state) (ivrs: 'r ivr list) : 'state ivr = 
        let arbiter state (r: 'r result) = 
            match r with
            | Value v -> arbiter state v
            | Error e -> cancelField (Error e)
            | Cancelled -> cancelField Cancelled

        field' arbiter initial ivrs

    /// Combine a list of ivrs so that they run in parallel. The resulting ivr ends when 
    /// all ivrs ended. When an error occurs in one of the ivrs, the resulting ivr ends with
    /// that error and all other ivrs are cancelled.

    let all (ivrs: 'r ivr list) : 'r list ivr = 

        // all can be implemented in terms of the field algorithm:

        // first attach indices
        let ivrs = ivrs |> lmapi (fun i r -> i, r)
        
        // arbiter collects the results in a map
        let arbiter state (i, r) = 
            let state = state |> Map.add i r
            continueField state []

        // and at last, convert the map to a list.
        let mapToList m = 
            // note: Map.toList returns a list that is ordered by the keys.
            m 
            |> Map.toList 
            |> List.map snd

        ivrs 
        |> field arbiter Map.empty
        |> map mapToList
    
    /// Runs a list of ivrs in parallel and finish with the first one that completes.
    /// If one ivr completes, it may take an arbitrary amount of time (steps) until the result is finally 
    /// returned, because the remaining ivrs may refuse to get cancelled.
    let any (ivrs: 'r ivr list) : 'r ivr =

        // Note: when an ivr finishes, all the running ones are canceled in the reversed 
        // order they were originally specified in the list 
        // (independent of how many of them already received the current event)!

        let arbiter _ = cancelField
        field' arbiter Unchecked.defaultof<'r> ivrs

    /// Runs two ivrs in parallel, the resulting ivr completes, when both ivrs are completed.
    /// Events are delivered first to ivr1, then to ivr2. When one of the ivrs terminates without a result 
    /// (cancellation or exception), the resulting ivr is ended immediately.

    /// Note that join retains the result of the first completed ivr, which
    /// could lead to leaks in nested parallel ivrs of which the result
    /// is never processed.

    // tbd: this does not belong into the core module.

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

    /// tbd: this does not belong into the core module.
    let first (ivr1 : 'r1 ivr) (ivr2 : 'r2 ivr) : Choice<'r1, 'r2> ivr =

        let ivr1 = ivr1 |> map Choice<_,_>.Choice1Of2
        let ivr2 = ivr2 |> map Choice<_,_>.Choice2Of2
        any [ivr1; ivr2]

    /// Runs three ivrs in parallel, the resulting ivr completes with the result of the one that finishes first.
    /// tbd: this does not belong into the core module.
    let first' (ivr1 : 'r1 ivr) (ivr2 : 'r2 ivr) (ivr3: 'r3 ivr) : Choice<'r1, 'r2, 'r3> ivr =

        let ivr1 = ivr1 |> map Choice<_,_,_>.Choice1Of3
        let ivr2 = ivr2 |> map Choice<_,_,_>.Choice2Of3
        let ivr3 = ivr3 |> map Choice<_,_,_>.Choice3Of3
        any [ivr1; ivr2; ivr3]

    //
    // Sending requests to the host.
    //

    /// Response type interface that is used to tag requests with that return a value. 
    /// Tag data types with this interface and use them as a request with IVR.send so that 
    /// IVR.send is able to cast the resulting value. Use IRequest<unit> for Requests that return
    /// no value.
    type IRequest<'response> = 
        interface end
    
    /// An IVR that synchronously sends a request to a host and returns its response. 
    let private sendUnsafe request : 'r ivr = 
        fun () ->
            Requesting (box request, Result.map unbox >> Completed)

    /// An IVR that synchronously sends a request to a host and returns its response. The requests
    /// need to implement the IRequest<_> interface so that the returned response value can be typed
    /// properly.
    let send (request: IRequest<'r>) : 'r ivr = 
        sendUnsafe request

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

    /// Attach a compensating ivr for the ivr body. That cancellation ivr is called 
    /// when the code inside the block gets cancelled. 
    let onCancel (compensation: unit ivr) = 
        continueWith <| function
        | Cancelled -> compensation |> continueWith (function
            | Value _ -> Cancelled |> ofResult
            | Error e -> e |> ofError
            // the compensation ivr got cancelled! This is an error for now!
            // tbd: An cancellation ivr must be protected from further cancellation.
            | Cancelled -> NestedCancellationException |> ofError)
        | r -> r |> ofResult

    /// Attach an compensating ivr for the ivr body that is called when a cancellation or an error
    /// happened.
    /// The compensating ivr receives (Some Exception) in case of an error, and None in case
    /// of a cancelation.
    let onCancelOrError (compensation: exn option -> unit ivr) body = 
        let compensate rBody =
            let afterCancel = function
            | Value _ -> rBody |> ofResult
            | Error e -> e |> ofError
            // the compensation ivr got cancelled! This is an error for now!
            // tbd: An cancellation ivr must be protected from further cancellation.
            | Cancelled -> NestedCancellationException |> ofError

            match rBody with
            | Error e -> compensation (Some e) |> continueWith afterCancel
            | Cancelled -> compensation None |> continueWith afterCancel
            | r -> r |> ofResult

        body |> continueWith compensate

    //
    // Wait primitives
    //

    /// An IVR that waits for some event given a function that returns (Some result) or None.
    let wait f =
        let rec waiter (ev: Event) =
            match ev with
            | :? TryCancel -> Cancelled |> Completed
            | _ ->
            try
                match f ev with
                | Some r -> r |> Value |> Completed
                | None -> Waiting waiter
            with e ->
                e |> Error |> Completed

        fun () -> Waiting waiter

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
    let idle<'r> : 'r ivr = 
        wait' (fun _ -> false)
        |> map (fun _ -> Unchecked.defaultof<'r>)

    //
    // Async Requests can be used for service based request response scenarios. 
    // These are preferable to inline async commands, because they can be made
    // serializable and traceable.
    //

    type IAsyncRequest<'response> = 
        interface end

    [<NoComparison>]
    type AsyncResponse<'response> = 
        | AsyncResponse of Id * 'response result

    /// Every IAsyncRequest handler needs to use this shared, thread-safe Id generator.
    let generateAsyncRequestId = Ids.newGenerator().GenerateId

    let sendAsync (cmd: IAsyncRequest<'response>) : 'response ivr =
        sendUnsafe cmd
        |> bind (fun id -> 
            waitFor (fun (AsyncResponse(responseId, result)) -> 
                if responseId = id then Some result else None))
        |> bind ofResult
        
    //
    // Computation expression builder for sequential IVR processes.
    //

    type IVRBuilder<'result>() = 

        member __.Source(ivr: _ ivr) : _ ivr = ivr
        member __.Source(r: IRequest<_>) = r |> send
        member __.Source(r: IAsyncRequest<_>) = r |> sendAsync
        member __.Source(s: _ seq) = s

        member __.Bind(ivr: 'r ivr, body: 'r -> 'r2 ivr) : 'r2 ivr = 
            ivr |> bind body

        member __.Return(v: 'r) : 'r ivr = 
            ofValue v
        member __.ReturnFrom(ivr : 'r ivr) = 
            ivr
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

                // note: f is already delayed
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
            
    /// Combine a number of unit ivrs so that they are run in sequence, one after another.
    let rec sequence (ivrs: unit ivr list) : unit ivr = ivr {
        match ivrs with
        | [] -> ()
        | next::rest ->
            do! next
            return! sequence rest
    }

    //
    // IVR System Requests & Events
    //

    type Delay = 
        | Delay of TimeSpan
        interface IRequest<Id>
    
    type CancelDelay = 
        | CancelDelay of Id
        interface IRequest<unit>

    type DelayCompleted = DelayCompleted of Id

    /// Wait for the given time span and continue then.
    let delay (ts: TimeSpan) = ivr {
        if ts < TimeSpan.Zero then
            failwithf "IVR.delay: unsupported negative time span: %s" (ts |> string)
        if ts <> TimeSpan.Zero then
            let! id = Delay ts
            try
                do! waitFor' (fun (DelayCompleted id') -> id' = id)
            finally
                CancelDelay id |> send
    }

    /// Deliver an event to the currently active processes.
    [<NoComparison>]
    type Schedule = 
        | Schedule of Event
        member this.Event = let (Schedule e) = this in e
        interface IRequest<unit>
    
    let schedule (e: Event) = 
        e |> Schedule |> send

    //
    // IVR System combinators
    //

    /// Process the ivr given and return it's value as an option or
    /// timeout after the given timespan and return None.
    let timeoutAfter (ts: TimeSpan) ivr = 
        any [
            // The delay gets started first, to ensure that
            // startup times of ivr are included in the measurement.
            delay ts |> map (fun () -> None)
            ivr |> map Some
        ]

    //
    // Async interoperability
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
    let await (computation: Async<'r>) : 'r ivr = ivr {
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

module GlobalExports = 
    let ivr<'r> = IVR.ivr<'r>
    type ivr<'r> = IVR.ivr<'r>
    /// Monadic bind, process a, and then process b with the result of a.
    let inline (>>=) a b = a |> IVR.bind b
    /// Same as >>=, but ignores the result of a.
    let inline (>>=.) a b = a |> IVR.bind (fun _ -> b)

[<assembly:AutoOpen("IVR.GlobalExports")>]
do ()

namespace Mom

open System
open System.Runtime.ExceptionServices
open Mom.Flux

(*
    An Mom is a definition of an asynchronous process with the following properties:
    
    - can be paused and ended at any time.
    - can synchronously wait and respond to events.
    - can be combined in parallel or sequentially.
    - can send any number of requests to a host.
    - can be run in steps.
*)

[<RequireQualifiedAccess>]
module Mom = 

    let inline (^) a b = (<|) a b

    [<NoComparison;NoEquality>]
    type 'result mom = unit -> 'result flux

    /// Start up an mom.
    let inline start (mom : _ mom) = mom ()

    /// Lifts a result.
    let inline ofResult (r: 'r result) : 'r mom = 
        fun () -> Completed r

    /// Lifts a value. Creates an Mom that returns the value.
    let inline unit (v: 'v) : 'v mom =
        ofResult ^ Value v

    /// Another way to lift a value.
    let inline ofValue (v: 'v) : 'v mom = 
        unit v
    
    /// Lifts an error.
    let inline ofError e : 'v mom =
        ofResult ^ Error e

    /// Continues the mom with a followup mom (Monad bind).
    let continueWith (followup : 'a result -> 'b mom) (mom: 'a mom) : 'b mom =

        let rec next = function
            | Requesting (req, cont) 
                -> Requesting (req, cont >> next)
            | Waiting cont 
                -> Waiting (cont >> next)
            | Completed r -> 
                try start ^ followup r
                with e -> Completed ^ captureException e

        fun () ->
            start mom |> next

    let inline bind body = 
        continueWith (Result.convert body ofError ofResult)
    
    /// Maps the mom's result. In other words: lifts a function that converts a value from a to b
    /// into the Mom category.
    let map (f: 'a -> 'b) (mom: 'a mom) : 'b mom = 
        let f (r: 'a result) = 
            fun () -> r |> Result.map f |> Completed
        continueWith f mom

    /// Ignores the mom's result.
    let inline ignore mom = mom |> map ignore

    /// Runs the mom, ignores its result, and then changes the mom's return value to the value given. 
    let inline force value mom = mom |> map (fun _ -> value)
            
    /// Invokes a function when the mom is completed.
    let inline whenCompleted f =
        continueWith (fun r -> f(); ofResult r)

    exception AsynchronousException of Why: string with
        override this.ToString() =
            sprintf "an Mom got into a waiting state, even though it is expected to run only synchronously(%s)" this.Why

    /// Test if the Mom runs only synchronously (never gets into a waiting state). This
    /// function is only active for DEBUG builds.
    let synchronous (why: string) (mom: 'a mom) : 'a mom = 
#if DEBUG
        let rec next = function
            | Requesting(req, cont) 
                -> Requesting(req, cont >> next)
            | Waiting _ 
                -> raise (AsynchronousException why)
            | Completed _ as flux 
                -> flux

        fun () ->
            start mom |> next
#else
        mom
#endif

    //
    // Mom Combinators
    //

    /// Maps a list of Mom's by applying the function f to each result.
    let internal lmap (f: 'a -> 'b) (moms: 'a mom list) : ('b mom list) = 
        moms |> List.map (map f)

    let internal lmapi (f: int -> 'a -> 'b) (moms: 'a mom list) : ('b mom list) = 
        moms |> List.mapi (f >> map)

    //
    // field
    //

    /// The priority with which new players should enter the field.
    ///
    /// Experimental.
    [<RequireQualifiedAccess; Struct>]
    type EnteringPriority = 
        /// Default: New players are immediately entering the field to replace the player that has exited before.
        | Now
        /// New players are added to field as soon all current players are in the waiting state.
        | Lazy

    [<NoComparison;NoEquality>]
    type ArbiterDecision<'state, 'r> = 
        | CancelField of 'state result
        | ContinueField of 'state * EnteringPriority * 'r mom list

    /// Cancel all remaining Moms and set the result of the field mom
    let inline cancelField r = CancelField r

    /// Continue the field with a new state and optionally add some new players / Moms to it.
    ///
    /// When the field does not contain any more active moms, the 'state is returned
    /// as a final result of the field mom.
    let inline continueField state moms = ContinueField(state, EnteringPriority.Now, moms)

    let inline continueFieldLazy state moms = ContinueField(state, EnteringPriority.Lazy, moms)

    type Arbiter<'state, 'r> = 'state -> 'r result -> (ArbiterDecision<'state, 'r>)

    // a waiting mom
    type private 'result waiting = Event -> 'result flux

    [<NoEquality; NoComparison>]
    type private Field<'state, 'r> = {
        State: 'state
        /// The current event and waiting moms that need to receive it.
        Pending: (Event * 'r waiting list) option
        /// All moms that got the event already or new moms, all in reversed order.
        /// Note that new moms that were added in response to an event don't receive that same event.
        Processed: 'r waiting list
    }

    /// A generic algorithm for running Moms in parallel.
    ///
    /// The field:
    ///
    /// All moms are processed in parallel, and as soon an mom completes, the arbiter is asked what to do.
    /// The arbiter can either decide to cancel the field and set a final result or
    /// continue the field with
    ///   an intermediate state/result
    ///   and a number of new moms that are added to the field.
    ///
    /// Notes:
    ///   - The arbiter does not get to be invoked again after it cancels the field.
    ///   - When the arbiter throws an exception, it's equivalent to cancelling the field with 
    ///     that exception as an error result.
    ///   - Cancellation is processed in reversed field insertion order.
    ///
    /// The arbiter implementation consists of three nested loops (from outer to inner):
    ///   - Event Loop
    ///     Delivers a number of events to _all_ the moms that are currently running in parallel.
    ///   - Processing Loop
    ///     Delivers one event to a list of currently active moms.
    ///   - Mom Loop
    ///     Runs one mom as long it can receive an event.

    let field' (arbiter: Arbiter<'state, 'r>) (initial: 'state) (moms: 'r mom list) : 'state mom = 

        let inline rev l = List.rev l

        // As long new Moms need to be added to the field we try to bring them into a state when they
        // want to receive events only.
        let rec enter (field: Field<'state, 'r>) = function
            | [] 
                // Finished adding new moms to the field, and all are in a waiting state now.
                -> proceed field
            | mom :: pending 
                // Start mom and continue pushing the remaining moms into the field.
                -> proceedMom field pending (start mom)
    
        // Proceed a mom until it lands in the waiting state, and add additional moms after that, if needed.
        and proceedMom (field: Field<'state, 'r>) newMoms = function
            | Requesting (request, cont) 
                -> Requesting (request, cont >> proceedMom field newMoms)
            | Waiting waiting
                // good, the current mom is waiting, 
                // so continue entering the remaining ones, and mark the 
                // current processed (which means that it does not receive the current event
                // if there is any).
                -> enter { field with Processed = waiting::field.Processed } newMoms
            | Completed result
                -> askArbiter field result newMoms

        and askArbiter (field: Field<'state, 'r>) (result: 'r result) (pending: 'r mom list) =

            // if the arbiter crashes, the field will be cancelled.
            let inline arbiter state r = 
                try
                    arbiter state r
                with e ->
                    CancelField ^ captureException e

            match arbiter field.State result with
            | ContinueField (newState, priority, newPlayers) ->
                let players =
                    match priority with
                    | EnteringPriority.Now -> newPlayers @ pending
                    | EnteringPriority.Lazy -> pending @ newPlayers
                enter { field with State = newState } players
            | CancelField result ->
                let processed = 
                    match field.Pending with
                    | Some(_, pending)
                        -> (rev pending) @ field.Processed
                    | None
                        -> field.Processed
                cancel result [] processed
        
        // Move the field forward.
        and proceed (field: Field<'state, 'r>) =
            match field.Pending with
            | Some (ev, pending) ->
                match pending with
                | [] -> proceed { field with Pending = None }
                | waiting::pending
                    // deliver the event and be sure that the mom is removed from pending
                    // interesting detail: Every mom that returns Requesting or Completed
                    // could add new moms without breaking the algorithm.
                    -> waiting ev |> proceedMom { field with Pending = Some (ev, pending) } [(* no new moms entering*)]
            | None ->
                // no event to process, start next waiting round.
                match field.Processed with
                | [] 
                    -> exit ^ Value field.State
                | processed 
                    -> Waiting (fun ev -> proceed { field with Pending = Some (ev, rev processed); Processed = [] })

        and cancel result cancelled = function
            | [] 
                -> finalizeCancel result cancelled
            | waiting::pending
                -> cancel result ((Flux.cancel (Waiting waiting))::cancelled) pending

        // Finalize the cancellation by processing all moms until they are completed, 
        // ... and then ignore their result.
        and finalizeCancel result pending =
            match pending with
            | [] -> exit result
            | flux::pending ->
            match flux with
            | Requesting (request, cont) 
                -> Requesting (request, cont >> fun flux -> finalizeCancel result (flux::pending))
            | Waiting _ 
                // See issue #4 why a mom that flips back to Waiting after a cancellation can not be supported.
                -> raise AsynchronousCancellationException
                // Waiting (cont >> fun flux -> finalize result (flux::pending))
            | Completed _ 
                -> finalizeCancel result pending

        and exit result =
            Completed result

        fun () ->
            enter { State = initial; Pending = None; Processed = [] } moms

    /// field is a simpler version of the field, in which errors automatically lead to
    /// the cancellation of the field so that the arbiter does not need to handle them.
    let field arbiter = 
        let arbiter state = Result.convert (arbiter state) (Error >> cancelField) cancelField
        field' arbiter

    /// Combine a list of moms so that they run in parallel. The resulting mom ends when 
    /// all moms ended. When an error occurs in one of the moms, the resulting mom ends with
    /// that error and all other moms are cancelled.
    let all (moms: 'r mom list) : 'r list mom = 

        // all can be implemented in terms of the field algorithm:

        // first attach indices
        let moms = moms |> lmapi (fun i r -> i, r)
        
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

        moms 
        |> field arbiter Map.empty
        |> map mapToList
    
    /// Runs a list of moms in parallel and finish with the first one that completes.
    ///
    /// If one mom completes, it may take an arbitrary amount of time (steps) until the result is
    /// finally returned, because the remaining moms may refuse to get cancelled.
    let race (moms: 'r mom list) : 'r mom =
        if List.isEmpty moms then
            failwith "internal error: Mom.race with zero nested moms would be unsound."

        // Note: when an mom finishes, all the running ones are canceled in the reversed 
        // order they were originally specified in the list 
        // (independent of how many of them already received the current event)!

        let arbiter _ = cancelField
        field' arbiter Unchecked.defaultof<'r> moms

    [<Obsolete("Use race")>]
    let inline any (moms: 'r mom list) : 'r mom = race moms

    /// Runs two moms in parallel, the resulting mom completes, when both moms are completed.
    /// Events are delivered first to mom1, then to mom2. When one of the moms terminates without a result 
    /// (cancellation or exception), the resulting mom is ended immediately.

    /// Note that join retains the result of the first completed mom, which
    /// could lead to leaks in nested parallel moms of which the result
    /// is never processed.

    // tbd: this does not belong into the core module.

    let join (mom1 : 'r1 mom) (mom2 : 'r2 mom) : ('r1 * 'r2) mom =

        // join is implemented in terms of all

        [map box mom1; map box mom2] 
        |> all
        |> map (function 
            | [l;r] -> unbox l, unbox r 
            | _ -> failwith "internal error")

    /// Runs two moms in parallel, the resulting mom completes with the result of the one that finishes first.
    /// events are delivered to mom1 and then to mom2, so mom1 has an advantage when both complete in response to
    /// the same event. Note that if mom1 completes, no event is delivered to mom2.

    /// tbd: this does not belong into the core module.
    let first (mom1 : 'r1 mom) (mom2 : 'r2 mom) : Choice<'r1, 'r2> mom =

        let mom1 = mom1 |> map Choice<_,_>.Choice1Of2
        let mom2 = mom2 |> map Choice<_,_>.Choice2Of2
        race [mom1; mom2]

    /// Runs three moms in parallel, the resulting mom completes with the result of the one that finishes first.
    /// tbd: this does not belong into the core module.
    let first' (mom1 : 'r1 mom) (mom2 : 'r2 mom) (mom3: 'r3 mom) : Choice<'r1, 'r2, 'r3> mom =

        let mom1 = mom1 |> map Choice<_,_,_>.Choice1Of3
        let mom2 = mom2 |> map Choice<_,_,_>.Choice2Of3
        let mom3 = mom3 |> map Choice<_,_,_>.Choice3Of3
        race [mom1; mom2; mom3]

    //
    // Sending requests to the host.
    //

    /// Response type interface that is used to tag requests with that return a value. 
    /// Tag data types with this interface and use them as a request with Mom.send so that 
    /// Mom.send is able to cast the resulting value. Use IRequest<unit> for Requests that return
    /// no value.
    type IRequest<'response> = 
        interface end
    
    /// An Mom that synchronously sends a request to a host and returns its response. 
    let private sendUnsafe request : 'r mom = 
        fun () ->
            Requesting (box request, Result.map unbox >> Completed)

    /// An Mom that synchronously sends a request to a host and returns its response. The requests
    /// need to implement the IRequest<_> interface so that the returned response value can be typed
    /// properly.
    let send (request: IRequest<'r>) : 'r mom = 
        sendUnsafe request

    //
    // IDisposable, Flow style
    //

    type IDisposableFlow =
        inherit IDisposable
        abstract member DisposableFlow : unit mom

    //
    // Cancellation Helper
    //

    /// Exception / Error that represents a nested cancellation error

    exception NestedCancellationException

    /// Attach a compensating mom for the mom body. That cancellation mom is called 
    /// when the code inside the block gets cancelled. 
    let onCancel (compensation: unit mom) = 
        continueWith <| function
        | Cancelled -> compensation |> continueWith (function
            | Value _ -> ofResult Cancelled
            | Error e -> ofError e
            // the compensation mom got cancelled! This is an error for now!
            // tbd: An cancellation mom must be protected from further cancellation.
            | Cancelled -> ofResult ^ captureException NestedCancellationException)
        | r -> ofResult r

    /// Attach an compensating mom for the mom body that is called when a cancellation or an error
    /// happened.
    /// The compensating mom receives (Some Exception) in case of an error, and None in case
    /// of a cancelation.
    let onCancelOrError (compensation: ExceptionDispatchInfo option -> unit mom) body = 
        let compensate rBody =
            let afterCancel = function
            | Value _ -> ofResult rBody
            | Error e -> ofError e
            // the compensation mom got cancelled! This is an error for now!
            // tbd: An cancellation mom must be protected from further cancellation.
            | Cancelled -> ofResult ^ captureException(NestedCancellationException)

            match rBody with
            | Error e -> compensation (Some e) |> continueWith afterCancel
            | Cancelled -> compensation None |> continueWith afterCancel
            | r -> ofResult r

        body |> continueWith compensate

    //
    // Wait primitives
    //

    /// An Mom that waits for some event given a function that returns (Some result) or None.
    let wait f =
        let rec waiter (ev: Event) =
            match ev with
            | :? Cancel -> Cancelled |> Completed
            | _ ->
            try
                match f ev with
                | Some r -> r |> Value |> Completed
                | None -> Waiting waiter
            with e ->
                e |> captureException |> Completed

        fun () -> Waiting waiter

    /// Waits for some event by asking a predicate for each event that comes along.
    /// Continues waiting when the predicate returns false, ends the mom when the predicate 
    /// returns true.
    let wait' predicate = 
        let f e = 
            match predicate e with
            | true -> Some ()
            | false -> None

        wait f

    /// Waits for an event of a type derived by the function f that is passed in. 
    /// Ends the mom with the value returned by the function if it returns (Some value), 
    /// continues waiting when f returns None
    let waitFor (f : 'e -> 'r option) = 

        let f (ev : Event) = 
            match ev with
            | :? 'e as e -> f e
            | _ -> None

        wait f

    /// Waits for an event of a type derived by the function f that is passed in.
    /// Ends the mom when f return true. Continues waiting when f returns false.
    let waitFor' (f : 'e -> bool) =
        let f (ev: Event) =
            match ev with
            | :? 'e as e -> f e
            | _ -> false

        wait' f

    /// Waits forever.
    let idle<'r> : 'r mom = 
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

    let sendAsync (cmd: IAsyncRequest<'response>) : 'response mom =
        sendUnsafe cmd
        |> bind (fun id -> 
            waitFor (fun (AsyncResponse(responseId, result)) -> 
                if responseId = id then Some result else None))
        |> bind ofResult
        
    //
    // Computation expression builder for sequential Mom processes.
    //

    type MomBuilder<'result>() = 

        member inline __.Source(mom: _ mom) : _ mom = mom
        member inline __.Source(r: IRequest<_>) = r |> send
        member inline __.Source(r: IAsyncRequest<_>) = r |> sendAsync
        member inline __.Source(s: _ seq) = s

        member inline __.Bind(mom: 'r mom, body: 'r -> 'r2 mom) : 'r2 mom = 
            mom |> bind body

        member inline __.Return(v: 'r) : 'r mom = 
            ofValue v
        member inline __.ReturnFrom(mom : 'r mom) = 
            mom
        member inline this.Delay(f : unit -> 'r mom) : 'r mom = 
            this.Bind(this.Return(), f)
        member inline this.Zero () : unit mom = 
            this.Return()

        member this.Using(disposable : 't, body : 't -> 'r mom when 't :> IDisposable) : 'r mom =
            let body = body disposable
            match box disposable with
            | :? IDisposableFlow as dp 
                -> this.TryFinally(body, fun () -> dp.DisposableFlow)
            | _ -> this.TryFinally(body, disposable.Dispose)

        member __.TryFinally(mom: 'r mom, f: unit -> unit mom) : 'r mom =
            let finallyBlock tryResult =

                // if the finally mom results in an error or cancellation, 
                // this _is_ our result, otherwise return the result.
                let afterFinally finallyResult =
                    match finallyResult with
                    | Value _ -> ofResult tryResult
                    | Error e -> ofError e
                    // if the finally {} got cancelled, our whole
                    // block's result is Cancelled
                    | Cancelled -> ofResult Cancelled

                // note: f is already delayed
                f() |> continueWith afterFinally

            mom 
            |> continueWith (finallyBlock >> synchronous "finally")

        member __.TryFinally(mom: 'r mom, f: unit -> unit) : 'r mom =
            mom |> whenCompleted f

        member __.TryWith(mom: 'r mom, eh: exn -> 'r mom) : 'r mom =
            mom |> continueWith (function
                | Error e 
                    -> eh e.SourceException
                | r -> ofResult r)
                
        member inline this.Combine(mom1: unit mom, mom2: 'r mom) : 'r mom =
            this.Bind(mom1, fun () -> mom2)

        // http://fsharpforfunandprofit.com/posts/computation-expressions-builder-part6/
        // While can be implemented in terms of Zero() and Bind()
        member this.While(guard: unit -> bool, body: unit mom) : unit mom =
            if not ^ guard()
            then this.Zero()
            else this.Bind(body, fun () -> this.While(guard, body))

        // For with Using(), While(), and Delay().
        member this.For(sequence: seq<'a>, body: 'a -> unit mom) : unit mom =
            this.Using(sequence.GetEnumerator(), fun enum ->
                this.While(enum.MoveNext,
                    this.Delay(fun () -> body enum.Current)))

    let mom<'result> = MomBuilder<'result>()

    /// Construct an IDisposableProc from a unit mom, so that this mom can be used
    /// with F# 'use' keyword inside a computation expression.
    let inline asDisposable (mom: unit mom) = { 
        new IDisposableFlow with
            member __.Dispose() = ()
            member __.DisposableFlow = mom
    }
            
    /// Combine a number of unit moms so that they are run in sequence, one after another.
    let rec sequence (moms: unit mom list) : unit mom = mom {
        match moms with
        | [] -> ()
        | next::rest ->
            do! next
            return! sequence rest
    }

    //
    // Mom System Requests & Events
    //

    type Delay = 
        | Delay of TimeSpan
        interface IRequest<Id>
    
    type CancelDelay = 
        | CancelDelay of Id
        interface IRequest<unit>

    type DelayCompleted = DelayCompleted of Id

    /// Wait for the given time span and continue then.
    let delay (ts: TimeSpan) = mom {
        if ts < TimeSpan.Zero then
            failwithf "Mom.delay: unsupported negative time span: %s" (ts |> string)
        if ts <> TimeSpan.Zero then
            let! id = Delay ts
            try
                do! waitFor' (fun (DelayCompleted id') -> id' = id)
            finally
                send ^ CancelDelay id
    }

    //
    // Mom System combinators
    //

    /// Process the mom given and return it's value as an option or
    /// timeout after the given timespan and return None.
    let timeoutAfter (ts: TimeSpan) mom = 
        race [
            // The delay gets started first, to ensure that
            // startup times of mom are included in the measurement.
            delay ts |> map (fun () -> None)
            mom |> map Some
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
                        Value ^ box r |> receiver
                    with e ->
                        captureException e |> receiver
                } |> Async.Start

        interface IRequest<Id>

    [<NoComparison>]
    type AsyncComputationCompleted = AsyncComputationCompleted of id: Id * result: obj result

    /// Waits for an F# asynchronous computation.
    let await (computation: Async<'r>) : 'r mom = mom {
        let! id = AsyncComputation(computation) |> send
        let! result = 
            waitFor(
                fun (AsyncComputationCompleted (id', result)) -> 
                    if id' = id then Some result else None)
        return! (result |> Result.map unbox) |> ofResult
    }

/// Helpers for mapping values of Mom lists
module Moms = 
    let map f moms = Mom.lmap f moms
    let mapi f moms = Mom.lmapi f moms

module GlobalExports = 
    let mom<'r> = Mom.mom<'r>
    type mom<'r> = Mom.mom<'r>
    /// Monadic bind, process a, and then process b with the result of a.
    let inline (>>=) a b = a |> Mom.bind b
    /// Same as >>=, but ignores the result of a.
    let inline (>>=.) a b = a |> Mom.bind (fun _ -> b)

[<assembly:AutoOpen("Mom.GlobalExports")>]
do ()

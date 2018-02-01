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

    let internal (^) = (<|)

    [<NoComparison;NoEquality>]
    type 'result mom = unit -> 'result flux

    /// Start up an mom.
    let start (mom : _ mom) = mom ()

    /// Lifts a result.
    let ofResult (r: 'r result) : 'r mom = 
        fun () -> Completed r

    /// Lifts a value. Creates an Mom that returns the value.
    let unit (v: 'v) : 'v mom =
        ofResult ^ Value v

    /// Another way to lift a value.
    let ofValue (v: 'v) : 'v mom = 
        unit v
    
    /// Lifts an error.
    let ofError e : 'v mom =
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

    let bind body = 
        continueWith (Result.convert body ofError ofResult)
    
    /// Maps the mom's result. In other words: lifts a function that converts a value from a to b
    /// into the Mom category.
    let map (f: 'a -> 'b) (mom: 'a mom) : 'b mom = 
        let f (r: 'a result) = 
            fun () -> r |> Result.map f |> Completed
        continueWith f mom

    /// Ignores the mom's result.
    let ignore mom = mom |> map ignore

    /// Runs the mom, ignores its result, and then changes the mom's return value to the value given. 
    let force value mom = mom |> map (fun _ -> value)
            
    /// Invokes a function when the mom is completed.
    let whenCompleted f =
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

    [<NoComparison;NoEquality>]
    type ArbiterDecision<'state, 'r> = 
        | CancelField of 'state result
        | ContinueField of 'state * 'r mom list

    /// Cancel all remaining Moms and set the result of the field mom
    let cancelField r = CancelField r

    /// Continue the field with a new state and optionally add some new players / Moms to it.
    /// Note: when the field does not contain any more active moms, the 'state is returned
    /// as a final result of the field mom.
    let continueField state moms = ContinueField(state, moms)

    type Arbiter<'state, 'r> = 'state -> 'r result -> (ArbiterDecision<'state, 'r>)

    [<NoEquality; NoComparison>]
    type private Field<'state, 'r> = {
        State: 'state
        Event: Response option
        Pending: 'r flux list
        /// All moms that got the event already or new moms, all in reversed order.
        Processed: 'r flux list
    }

    /// A generic algorithm for running Moms in parallel.
    /// The field:
    /// All moms are processed in parallel, and as soon an mom completes, the arbiter is asked what to do.
    /// The arbiter can either decide to cancel the field and set a final result or
    /// continue the field with a 
    ///   an intermediate state/result
    ///   and a number of new moms to add to the field.
    /// Notes:
    ///   The arbiter does not get to be asked again, after it cancels the field.
    ///   When the arbiter throws an exception, it's equivalent to cancelling the field with 
    ///   that exception as an error result.
    ///   Cancellation is processed in reversed field insertion order.
    
    let field' (arbiter: Arbiter<'state, 'r>) (initial: 'state) (moms: 'r mom list) : 'state mom = 

        let rev = List.rev

        // if the arbiter crashes, the field will be cancelled.
        let arbiter state r = 
            try
                arbiter state r
            with e ->
                CancelField ^ captureException e

        /// As long new Moms need to be added to the field we try to bring them into a state when they
        /// want to receive events only.
        let rec enter (field: Field<'state, 'r>) pending =
            match pending with
            | [] 
                -> proceed field
            | mom :: pending 
                -> enter2 field (start mom) pending
        
        and enter2 (field: Field<'state, 'r>) flux pending =
            match flux with
            | Requesting (request, cont) 
                -> Requesting (request, cont >> fun flux -> enter2 field flux pending)
            | Waiting _ 
                // as long new moms are added to the field, event processing is delayed.
                -> enter { field with Processed = flux::field.Processed } pending
            | Completed result ->
            match arbiter field.State result with
            | ContinueField (newState, moreMoms) 
                -> enter { field with State = newState } (moreMoms @ pending)
            | CancelField result 
                -> cancel result [] ((rev field.Pending) @ field.Processed)
        
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
                | Requesting _ 
                    -> failwithf "internal error: %A in field pending" flux
                | Waiting cont 
                    // deliver the event and be sure that the mom is removed from pending
                    ->
                    cont ev |> postProcess { field with Pending = pending }

        // Continue processing the field or ask the arbiter what to do if the mom is completed.
        and postProcess (field: Field<'state, 'r>) flux =
            match flux with
            | Requesting (request, cont) 
                -> Requesting (request, cont >> fun flux -> postProcess field flux)
            | Waiting _ 
                -> proceed { field with Processed = flux::field.Processed }
            | Completed result ->
            match arbiter field.State result with
            | ContinueField (newState, newMoms) 
                -> enter { field with State = newState } newMoms
            | CancelField result 
                -> cancel result [] ((rev field.Pending) @ field.Processed)

        // Cancellation:
        // send Cancel as soon they are in waiting state, if not, process host requests until they are.
        and cancel result cancelled pending =
            match pending with
            | [] -> finalize result cancelled
            | flux::pending ->
            match flux with
            | Requesting (request, cont) 
                -> Requesting(request, cont >> fun flux -> cancel result cancelled (flux::pending))
            | Waiting _ 
                -> cancel result ((Flux.cancel flux)::cancelled) pending
            | Completed _ 
                -> cancel result cancelled pending

        // Finalization: run them until they are all completed.
        and finalize result pending =
            match pending with
            | [] -> exit result
            | flux::pending ->
            match flux with
            | Requesting (request, cont) 
                -> Requesting (request, cont >> fun flux -> finalize result (flux::pending))
            | Waiting _ 
                // See issue #4 why Waiting can not be supported.
                -> raise AsynchronousCancellationException
                // Waiting (cont >> fun flux -> finalize result (flux::pending))
            | Completed _ 
                -> finalize result pending

        and exit result =
            Completed result

        fun () ->
            enter { State = initial; Event = None; Pending = []; Processed = [] } moms

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
    /// If one mom completes, it may take an arbitrary amount of time (steps) until the result is finally 
    /// returned, because the remaining moms may refuse to get cancelled.
    let any (moms: 'r mom list) : 'r mom =

        // Note: when an mom finishes, all the running ones are canceled in the reversed 
        // order they were originally specified in the list 
        // (independent of how many of them already received the current event)!

        let arbiter _ = cancelField
        field' arbiter Unchecked.defaultof<'r> moms

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
        any [mom1; mom2]

    /// Runs three moms in parallel, the resulting mom completes with the result of the one that finishes first.
    /// tbd: this does not belong into the core module.
    let first' (mom1 : 'r1 mom) (mom2 : 'r2 mom) (mom3: 'r3 mom) : Choice<'r1, 'r2, 'r3> mom =

        let mom1 = mom1 |> map Choice<_,_,_>.Choice1Of3
        let mom2 = mom2 |> map Choice<_,_,_>.Choice2Of3
        let mom3 = mom3 |> map Choice<_,_,_>.Choice3Of3
        any [mom1; mom2; mom3]

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
            | Cancelled -> ofError NestedCancellationException)
        | r -> r |> ofResult

    /// Attach an compensating mom for the mom body that is called when a cancellation or an error
    /// happened.
    /// The compensating mom receives (Some Exception) in case of an error, and None in case
    /// of a cancelation.
    let onCancelOrError (compensation: exn option -> unit mom) body = 
        let compensate rBody =
            let afterCancel = function
            | Value _ -> ofResult rBody
            | Error e -> ofError e
            // the compensation mom got cancelled! This is an error for now!
            // tbd: An cancellation mom must be protected from further cancellation.
            | Cancelled -> ofError NestedCancellationException

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

        member __.Source(mom: _ mom) : _ mom = mom
        member __.Source(r: IRequest<_>) = r |> send
        member __.Source(r: IAsyncRequest<_>) = r |> sendAsync
        member __.Source(s: _ seq) = s

        member __.Bind(mom: 'r mom, body: 'r -> 'r2 mom) : 'r2 mom = 
            mom |> bind body

        member __.Return(v: 'r) : 'r mom = 
            ofValue v
        member __.ReturnFrom(mom : 'r mom) = 
            mom
        member this.Delay(f : unit -> 'r mom) : 'r mom = 
            this.Bind(this.Return(), f)
        member this.Zero () : unit mom = 
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
                | Error (:? CapturedException as ci)
                    -> eh (ci.DispatchInfo.SourceException)
                | Error e 
                    -> eh e
                | r -> ofResult r)
                
        member this.Combine(mom1: unit mom, mom2: 'r mom) : 'r mom =
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
    let asDisposable (mom: unit mom) = { 
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
        any [
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

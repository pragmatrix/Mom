namespace Mom

module Flux =

    [<Struct;NoComparison>]
    type 'value result =
        | Value of v: 'value
        | Error of e: exn
        | Cancelled

    [<RequireQualifiedAccess>]
    module Result =
        let inline convert fv fe fc = function
            | Value r -> fv r
            | Error e -> fe e
            | Cancelled -> fc Cancelled

        let inline map f = function
            | Value r -> Value (f r)
            | Error e -> Error e
            | Cancelled -> Cancelled

    type Request = obj
    type Response = obj
    type Event = obj

    [<NoComparison;NoEquality>] 
    type 'result flux =
        | Requesting of Request * (Response result -> 'result flux)
        | Waiting of (Event -> 'result flux)
        | Completed of 'result result

    /// Dispatch an event to the flux that is currently waiting for one.
    let dispatch ev = function
        | Waiting cont -> cont ev
        | flux -> failwithf "Mom.dispatch: can't dispatch an event to an mom that is not waiting for one: %A" flux

    //
    // Completed state queries
    //
        
    /// Returns true if the flux is completed (i.e. has a result).
    let isCompleted = function
        | Completed _ -> true
        | _ -> false

    let isError = function
        | Completed (Error _) -> true
        | _ -> false

    /// Returns the error of a completed flux.
    let error = function
        | Completed (Error e) -> e
        | flux -> failwithf "Flux.error: flux is not in error: %A" flux

    /// The result of a completed flux.
    let result = function
        | Completed r -> r
        | flux -> failwithf "Flux.result: flux is not completed: %A" flux

    /// Returns the resulting value of a completed flux.
    let value = function
        | Completed (Value r) -> r
        | Completed _ as flux -> failwithf "Flux.value: flux has not been completed with a resulting value: %A" flux
        | flux -> failwithf "Flux.value: flux is not completed: %A" flux

    //
    // Cancellation
    //

    let isCancelled = function
        | Completed Cancelled -> true
        | _ -> false

    /// The event that is sent to a flux when it gets cancelled.
    type Cancel = Cancel

    exception AsynchronousCancellationException with
        override __.ToString() =
            sprintf "an Mom got into a waiting state, even though it is being cancelled and expected to run only synchronously"

    /// Cancels the flux. This actually sends a Cancel event to the flux, and expects it to get
    /// either in the Error or Cancelled state.
    /// The flux is required to react on Cancel events (for example every wait function does that)
    /// immediately and synchronously.
    /// If a flux is completed, the result is overwritten (freed indirectly) with a Cancelled error,
    /// but an error is not, to avoid shadowing the error.
    /// A flux that is delayed (not started yet) or waiting for a synchronous response, can not be cancelled.
    let cancel = 
        let rec next = function
            | Requesting(req, cont) 
                -> Requesting(req, cont >> next)
            | Waiting _ 
                -> raise AsynchronousCancellationException
            | Completed(Error _) as flux
                -> flux
            | Completed _ 
                -> Completed Cancelled

        function
        | Requesting _ 
            -> failwith "failed to cancel a flux that is requesting a synchronous response"
        | Waiting cont 
            -> cont Cancel |> next
        | Completed(Error _) as flux
            -> flux
        | Completed _ 
            -> Completed Cancelled

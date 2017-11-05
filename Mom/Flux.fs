namespace IVR

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

module Flux =

    type Request = obj
    type Response = obj
    type Event = obj

    [<NoComparison;NoEquality>] 
    type 'result flux =
        | Requesting of Request * (Response result -> 'result flux)
        | Waiting of (Event -> 'result flux)
        | Completed of 'result result

    /// Dispatch an event to the flux that is currently waiting for one.
    let dispatch ev flux =
        match flux with
        | Waiting cont -> cont ev
        | flux -> failwithf "IVR.dispatch: can't dispatch an event to an ivr that is not waiting for one: %A" flux

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
    let error flux = 
        match flux with
        | Completed (Error e) -> e
        | _ -> failwithf "Flux.error: flux is not in error: %A" flux

    /// The result of a completed flux.
    let result flux = 
        match flux with
        | Completed r -> r
        | _ -> failwithf "Flux.result: flux is not completed: %A" flux

    /// Returns the resulting value of a completed flux.
    let value flux = 
        match flux with
        | Completed (Value r) -> r
        | Completed _ -> failwithf "Flux.value: flux has not been completed with a resulting value: %A" flux
        | _ -> failwithf "Flux.value: flux is not completed: %A" flux

    //
    // Cancellation
    //

    let isCancelled flux = 
        match flux with
        | Completed Cancelled -> true
        | _ -> false


    /// The event that is sent to a flux when it gets cancelled.
    type Cancel = Cancel

    exception AsynchronousCancellationException with
        override this.ToString() =
            sprintf "an IVR got into a waiting state, even though it is being cancelled and expected to run only synchronously"

    /// Cancels the flux. This actually sends a Cancel event to the flux, and expects it to get
    /// either in the Error or Cancelled state.
    /// The flux is required to react on Cancel events (for example every wait function does that)
    /// immediately and synchronously.
    /// If a flux is completed, the result is overwritten (freed indirectly) with a Cancelled error,
    /// but an error is not, to avoid shadowing the error.
    /// A flux that is delayed (not started yet) or waiting for a synchronous response, can not be cancelled.
    let cancel flux = 
        let rec next flux = 
            match flux with
            | Requesting (req, cont) -> 
                Requesting(req, cont >> next)
            | Waiting _ ->
                raise AsynchronousCancellationException
            | Completed (Error _) -> flux
            | Completed _ -> Cancelled |> Completed

        match flux with
        | Requesting _ ->
            failwith "failed to cancel a flux that is requesting a synchronous response"
        | Waiting cont -> 
            cont Cancel |> next
        | Completed (Error _) -> flux
        | Completed _ -> Cancelled |> Completed

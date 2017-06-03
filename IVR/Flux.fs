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



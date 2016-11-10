/// A service that can be used to register async functions.
module IVR.AsyncService

[<NoEquality; NoComparison>]
type AsyncServiceBuilder = {
    IdGenerator: Ids.Generator
    Service: Runtime.IServiceContext -> Request -> Response option
}

/// Create a new async service builder.
let createBuilder() = { 
    IdGenerator = Ids.newGenerator()
    Service = fun _ _ -> None
}

/// Return the service from the builder.
let service builder = builder.Service

let private addDispatcher dispatcher (builder: AsyncServiceBuilder) = 

    { builder with
        Service = fun context request ->
            match builder.Service context request with
            | Some response -> Some response
            | None -> dispatcher context request
    }

/// Add an async service function to the builder.
let add (f: 'e -> Async<'response> when 'e :> IVR.IAsyncRequest<'response>) (builder: AsyncServiceBuilder) =
        
    let dispatch (context: Runtime.IServiceContext) : (Request -> Response option) =
        function
        | :? 'e as r -> 
            let id = builder.IdGenerator.GenerateId()
            Async.Start <| async {
                try
                    let! response = f r
                    IVR.AsyncResponse(id, Value response)
                    |> context.ScheduleEvent
                with e ->
                    IVR.AsyncResponse(id, Error e)
                    |> context.ScheduleEvent
            }
            Some <| box id
        | _ -> 
            None

    addDispatcher dispatch builder

/// Add an async service function that is considered unsafe, meaning that
/// its result is ignored. This is useful for situations in which
/// and async service function may be used in a finalizer (which can never
/// wait for an event / result), _and_ it's failure is not considered fatal.
let addUnsafe (f: 'e -> Async<unit> when 'e :> IVR.IRequest<unit>) =

    let dispatch _ : (Request -> Response option) =
        function
        | :? 'e as r -> 
            Async.Start <| async {
                try
                    do! f r
                with _ ->
                    // tbd: can we log that somehow?
                    ()
            }
            Some <| box ()
        | _ -> 
            None

    addDispatcher dispatch

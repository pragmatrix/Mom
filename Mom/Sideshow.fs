/// The idea:
/// We want to control another mom independently of the current mom we are running now.
/// The coordination should not be processed via channels, because it does not have to
/// pollute the trace and should not require a host.
///
/// There are two moms, The sideshow mom and the control mom. The control mom can start and
/// replace the sideshow mom. The control mom receives the propagated errors from the side
/// show mom. When a sideshow mom is started or replaced, the replace command does not
/// return until either the sideshow is in a waiting state or completed. This guarantees that
/// the control mom can be sure that all teardown of the old sideshow mom and startup of the
/// new one is processed when the control is returned to it.

[<RequireQualifiedAccess>]
module Mom.Sideshow

open Mom.GlobalExports

/// This is the control interface for the side show.

type Control<'state>(doBegin: 'state * unit mom -> unit mom, getState: unit -> 'state option mom) =
    /// Cancel and replace the side show mom that is tagged with the given state. 
    /// This mom returns after the
    /// currently running side show mom is cancelled and the new one is started (gets into
    /// a waiting state or completed). Errors are propagated to the mom of the caller.
    member this.Begin(state, mom) = doBegin(state, mom)
    /// Returns the state of the sideshow, or None if the sideshow is not active.
    member this.State with get() = getState()

type Control<'state> with
    /// End the sideshow by cancelling the current mom, which sets the state to None.
    member this.End() = 
        this.Begin(Mom.unit())
    /// Begin the sideshow with the default state.
    member this.Begin(mom: unit mom) : unit mom =
        this.Begin(Unchecked.defaultof<'state>, mom)

[<AutoOpen>]
module private Private = 
    let generateId = Ids.newGenerator().GenerateId

    [<NoEquality;NoComparison>]
    type Request<'state> = 
        | Replace of Id * ('state * unit mom)
        | GetState of Id

    type 'r flux = 'r Flux.flux

/// Attach a sideshow to a control mom that can control a sideshow mom.
let attachTo (control: Control<'state> -> 'r mom) : 'r mom =

    let communicationId = generateId()

    let replace (state : 'state, toReplace : unit mom) : unit mom = 
        fun () ->
            let request = Replace(communicationId, (state, toReplace))

            let processResponse (response: obj Flux.result) =
                match response with
                | Flux.Value _ -> Flux.Value ()
                | Flux.Error e -> Flux.Error e
                | Flux.Cancelled -> Flux.Cancelled
                |> Flux.Completed

            Flux.Requesting (request, processResponse)

    let getState() : 'state option mom = 
        fun () ->
            let request : Request<'state> = GetState(communicationId)

            let processResponse (response: obj Flux.result) =
                match response with
                | Flux.Value v -> Flux.Value (unbox v)
                | Flux.Error e -> Flux.Error e
                | Flux.Cancelled -> Flux.Cancelled
                |> Flux.Completed

            Flux.Requesting(request, processResponse)            

    let sideshowControl = Control<'state>(replace, getState)

    let isOurs = function
        | Replace(cid, _)
        | GetState(cid) when communicationId = cid 
            -> true
        | _ -> false

    let idleSideshow = Flux.Completed (Flux.Value ())

    // tbd: Flux module candidate!
    let cancelAndContinueWith continuation flux =
        let rec cancel flux =
            match flux with
            | Flux.Waiting _ ->
                Flux.cancel flux |> cancel
            | Flux.Requesting(r, cont) ->
                Flux.Requesting(r, cont >> cancel)
            | Flux.Completed result ->
                continuation result

        cancel flux

    // we wrap the control mom so that we can process the requests.

    fun () ->
        let control = 
            control sideshowControl 
            |> Mom.start

        let rec next (sideshow: 'state option * unit flux) (control: 'r flux) = 
            // sideshow has priority, so run it as long we can.
            let state, flux = sideshow
            match flux with 
            | Flux.Requesting(r, cont) -> 
                Flux.Requesting(r, cont >> fun flux -> next (state, flux) control)
            | _ ->
            // sideshow is either waiting or completed, proceed with the control mom
            match control with
            | Flux.Requesting(:? Request<'state> as request, cont) when isOurs request ->
                match request with
                | Replace(_, newSideshow) ->
                    beginSideshow sideshow newSideshow cont
                | GetState(_) ->
                    cont (Flux.Value (box state)) 
                    |> next sideshow
                    
            | Flux.Requesting(r, cont) ->
                Flux.Requesting(r, cont >> next sideshow)
            | Flux.Completed cResult ->
                flux |> cancelAndContinueWith (fun sResult ->
                    Flux.Completed <| 
                        // be sure errors of the control mom have precendence!
                        match cResult, sResult with
                        | Flux.Error c, _ -> Flux.Error c
                        | _, Flux.Error s -> Flux.Error s
                        | _ -> cResult
                    )
            | Flux.Waiting cont ->
                Flux.Waiting (fun event ->
                    match flux with
                    | Flux.Waiting sideshowCont ->
                        // deliver the event to both.
                        next (state, sideshowCont event) (cont event)
                    | _ ->
                        next sideshow (cont event))

        and beginSideshow sideshow (newState, newSideshow) contControl =

            // start the new sideshow (until we are in a waiting or completed state)
            let rec startNew (state, sideshow) =
                match sideshow with
                | Flux.Requesting(r, cont) ->
                    Flux.Requesting(r, cont >> (fun flux -> startNew(state, flux)))
                | Flux.Waiting _ ->
                    contControl (Flux.Value (box ()))
                    |> next (state, sideshow)
                | Flux.Completed result ->
                    result 
                    |> Flux.Result.map box
                    |> contControl
                    |> next (None, idleSideshow)

            snd sideshow
            |> cancelAndContinueWith
                (function
                | Flux.Error err ->
                    // sideshow was in error before or after the cancellation, 
                    // we propagate this to the Replace invoker and ignore the
                    // new sideshow
                    Flux.Error err
                    |> contControl
                    |> next (None, idleSideshow)

                | _ -> startNew (Some newState, newSideshow()))

        next (None, idleSideshow) control

type Control = Control<unit>

module Extensions = 
    module Mom =
        let withSideshow = attachTo

[<assembly:AutoOpen("Mom.Sideshow.Extensions")>]
do ()


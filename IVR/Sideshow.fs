/// The idea:
/// We want to control another ivr independently of the current ivr we are running now.
/// The coordination should not be processed via channels, because it does not have to
/// pollute the trace and should not require a host.
///
/// There are two ivrs, The sideshow ivr and the control ivr. The control ivr can start and
/// replace the sideshow ivr. The control ivr receives the propagated errors from the side
/// show ivr. When a sideshow ivr is started or replaced, the replace command does not
/// return until either the sideshow is in a waiting state or completed. This guarantees that
/// the control ivr can be sure that all teardown of the old sideshow ivr and startup of the
/// new one is processed when the control is returned to it.

[<RequireQualifiedAccess>]
module IVR.Sideshow

open IVR.GlobalExports

/// This is the control for the side show.

[<NoEquality; NoComparison>]
type Control = {
    /// Cancel and replace the side show ivr. This ivr returns after the
    /// currently running side show ivr is cancelled and the new one is started (gets into
    /// a waiting state or completed). Errors are propagated to the ivr of the caller.
    Replace: unit ivr -> unit ivr
}

[<AutoOpen>]
module private Private = 
    let generateId = Ids.newGenerator().GenerateId

    [<NoEquality;NoComparison>]
    type Request = 
        | Replace of Id * unit ivr

    type 'r flux = 'r IVR.flux

/// Run a control ivr that can control a sideshow ivr.
let run (control: Control -> 'r ivr) : 'r ivr =

    let communicationId = generateId()

    let replace (toReplace : unit ivr) : unit ivr = 
        fun () ->
            let request = Replace(communicationId, toReplace)

            let processResponse (response: obj IVR.result) =
                let result = 
                    match response with
                    | IVR.Value _ -> IVR.Value ()
                    | IVR.Error e -> IVR.Error e
                    | IVR.Cancelled -> IVR.Cancelled
                IVR.Completed result

            IVR.Requesting (request, processResponse)

    let sideshowControl = {
        Replace = replace
    }

    let isOurs (Replace(cid, _)) =
        communicationId = cid

    // tbd: Flux module candidate!
    let cancelAndContinueWith continuation flux =
        let rec cancel flux =
            match flux with
            | IVR.Waiting _ ->
                IVR.tryCancel flux |> cancel
            | IVR.Requesting(r, cont) ->
                IVR.Requesting(r, cont >> cancel)
            | IVR.Completed result ->
                continuation result

        cancel flux

    // we wrap the control ivr so that we can process the requests.

    fun () ->
        let control = 
            control sideshowControl 
            |> IVR.start

        let rec next (sideshow: unit flux) (control: 'r flux) = 
            // sideshow has priority, so run it as long we can.
            match sideshow with 
            | IVR.Requesting(r, cont) -> 
                IVR.Requesting(r, cont >> fun sideshow -> next sideshow control)
            | _ ->
            // sideshow is either waiting or completed, proceed with the control ivr
            match control with
            | IVR.Requesting(:? Request as request, cont) when isOurs request ->
                let (Replace(_, newSideshow)) = request
                replace sideshow newSideshow cont
            | IVR.Requesting(r, cont) ->
                IVR.Requesting(r, cont >> next sideshow)
            | IVR.Completed result ->
                sideshow |> cancelAndContinueWith (fun _ -> IVR.Completed result)
            | IVR.Waiting cont ->
                IVR.Waiting (fun event ->
                    match sideshow with
                    | IVR.Waiting sideshowCont ->
                        // deliver the event to both.
                        next (sideshowCont event) (cont event)
                    | _ ->
                        next sideshow (cont event))

        and replace sideshow newSideshow contControl =

            // start the new sideshow (until we are in a waiting or completed state)
            let rec startNew sideshow =
                match sideshow with
                | IVR.Requesting(r, cont) ->
                    IVR.Requesting(r, cont >> startNew)
                | IVR.Waiting _ ->
                    box ()
                    |> IVR.Value
                    |> contControl
                    |> next sideshow
                | IVR.Completed result ->
                    result 
                    |> IVR.Result.map (fun _ -> box ())
                    |> contControl
                    |> next sideshow

            sideshow
            |> cancelAndContinueWith
                (function
                | IVR.Error err ->
                    // sideshow was in error before or after the cancellation, 
                    // we propagate this to the Replace invoker and ignore the
                    // new sideshow
                    IVR.Error err
                    |> contControl
                    |> next (IVR.Completed (IVR.Value ()))

                | _ -> startNew (newSideshow()))

        next (IVR.Completed (IVR.Value ())) control

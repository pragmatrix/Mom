/// The idea:
/// We want to control another ivr independently of the current ivr we are running now.
/// The coordination should not be processed via channels, because it does not have to
/// pollute the trace and should not require a host.
///
/// The features we want:
/// - Synchronously replace the sideshow ivr.

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

    type Response = 
        | Response of Id

    type 'r flux = 'r IVR.flux

/// Run a nested ivr that can control a sideshow ivr.
let run (nested: Control -> 'r ivr) : 'r ivr =

    let communicationId = generateId()

    let replace (toReplace : unit ivr) : unit ivr = 
        fun () ->
            let request = Replace(communicationId, toReplace)

            let processResponse (response: obj IVR.result) =
                let result = 
                    match response with
                    | IVR.Value (:? Response as response) when (Response communicationId = response) ->
                        IVR.Value ()
                    | IVR.Value _ -> failwith "internal error, expected a dedicated response"
                    | IVR.Error e -> IVR.Error e
                    | IVR.Cancelled -> IVR.Cancelled
                IVR.Completed result

            IVR.Requesting (request, processResponse)

    let control = {
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
            | IVR.Completed _ ->
                continuation()

        cancel flux

    // we wrap the nested ivr so that we can process the requests.

    fun () ->
        let nested = 
            nested control 
            |> IVR.start

        let rec next (sideshow: unit flux) (nested: 'r flux) = 
            // sideshow has priority, so run it as long we can.
            match sideshow with 
            | IVR.Requesting(r, cont) -> 
                IVR.Requesting(r, cont >> fun sideshow -> next sideshow nested)
            | _ ->
            // sideshow is either waiting or completed, run nested
            // (note: sideshow errors are currently ignored)
            match nested with
            | IVR.Requesting(:? Request as request, cont) when isOurs request ->
                let (Replace(_, newSideshow)) = request
                replace sideshow newSideshow cont
            | IVR.Requesting(r, cont) ->
                IVR.Requesting(r, cont >> next sideshow)
            | IVR.Completed result ->
                sideshow |> cancelAndContinueWith (fun () -> IVR.Completed result)
            | IVR.Waiting cont ->
                IVR.Waiting (fun event ->
                    match sideshow with
                    | IVR.Waiting sideshowCont ->
                        // deliver the event to both.
                        next (sideshowCont event) (cont event)
                    | _ ->
                        next sideshow (cont event))

        and replace sideshow newSideshow contNested =

            // start the new sideshow (until we are in a waiting or completed state)
            let rec startNew sideshow =
                match sideshow with
                | IVR.Requesting(r, cont) ->
                    IVR.Requesting(r, cont >> startNew)
                | _ ->
                    Response communicationId
                    |> box
                    |> IVR.Value
                    |> contNested
                    |> next sideshow

            sideshow
            |> cancelAndContinueWith (fun () -> startNew (newSideshow()))

        next (IVR.Completed (IVR.Value ())) nested

/// Promiscuous IVRs are IVRs that can be controlled externally to a certain degree.
module IVR.Promiscuous

open IVR.GlobalExports

/// Implement this interface to a serializable type (a DU for example) to associate the ivr 
/// that should be run inside the promiscuous ivr.
type IIVR<'r> = 
    abstract IVR : 'r ivr

/// The (serializable) commands that can be sent to the promiscuous ivr.
type Command<'followup, 'rr> when 'followup :> IIVR<'rr> =
    /// Cancel the ivr and continue with the followup one.
    | Replace of 'followup

[<AutoOpen>]
module private Private =

    /// Send a command to a sender side of a promiscuous ivr.
    let post cmd (channel: Command<_, _> Channel.sender) =
        Channel.post cmd channel

    let generateNewId = Ids.newGenerator().GenerateId

let replace iivr = 
    Channel.post (Replace iivr)

/// Convert an ivr to a promiscuous ivr. Returns a command sender instance first 
/// and the promiscuous ivr second.
let make (startup: 'r ivr) : Command<_, _> Channel.sender * 'r ivr =

    // be sure the channel to create is unique (note: this is the first time we generate an id
    // from within an ivr instead of inside a service, I am not sure if this is a good idea)
    let id = generateNewId()

    let sender, receiver = Channel.create("promiscuous-" + string id.Value)

    let rec run (current: 'r ivr) : 'r ivr = ivr {
        let! r = 
            IVR.any [
                Channel.wait receiver |> IVR.map Choice1Of2
                current |> IVR.map Choice2Of2
            ]

        match r with
        | Choice1Of2 (Replace followup) ->
            return! run followup.IVR
        | Choice2Of2 result ->
            return result
    }

    sender, run startup



/// Interruptible IVRs are IVRs that can be interrupted and forced to continue with a new IVR.
[<RequireQualifiedAccess>]
module IVR.Interruptible

open IVR.GlobalExports

/// The (serializable) message that is internally sent to the ivr. This is public, because
/// it may be required for the serialization of ivr traces.
type Message<'command> =
    /// Cancel the ivr and continue with the followup one.
    | Replace of 'command

[<AutoOpen>]
module private Private =

    let generateNewId = Ids.newGenerator().GenerateId

/// Create a interruptible ivr that runs IVR.idle initially. 
///
/// Pass in a function that associates a serializable instance of a type with the
/// ivr the interruptible ivr should continue with.
///
/// Returns a function that allows to interrupt the ivr by sending an instance 
/// of a command type, and the interruptible ivr.
let create (producer: 'command -> 'r ivr) : ('command -> unit ivr) * 'r ivr =

    // be sure the channel to create is unique (note: this is the first time we generate an id
    // from within an ivr instead of inside a service, I am not sure if this is a good idea)
    let id = generateNewId()
    let sender, receiver = Channel.create("interruptible-" + string id.Value)

    let rec run (current: 'r ivr) : 'r ivr = ivr {

        let! r = 
            IVR.any [
                Channel.wait receiver |> IVR.map Choice1Of2
                current |> IVR.map Choice2Of2
            ]

        match r with
        | Choice1Of2 (Replace followup) ->
            return! run (producer followup)
        | Choice2Of2 result ->
            return result
    }

    let post cmd =
        Channel.post (Replace cmd) sender

    post, run IVR.idle

/// Create an interruptible ivr that is indempotent, meaning that it does not interrupt and
/// run a new ivr when the command that is sent is the same as the previous one.
// tbd: generalize this further, so that the producer can decide in each state transition
// if the state change should happen or not.
let createIndempotent (producer: 'command -> 'r ivr) : ('command -> unit ivr) * 'r ivr =

    // be sure the channel to create is unique (note: this is the first time we generate an id
    // from within an ivr instead of inside a service, I am not sure if this is a good idea)
    let id = generateNewId()
    let sender, receiver = Channel.create("interruptible-" + string id.Value)

    let rec run (currentCmd: 'command option) : 'r ivr = ivr {

        let current = 
            match currentCmd with
            | Some cmd -> producer cmd
            | None -> IVR.idle

        let shouldInterrupt (Replace cmd) = 
            if (Some cmd) <> currentCmd
            then Some cmd
            else None

        let! r = 
            IVR.any [
                Channel.wait' shouldInterrupt receiver |> IVR.map Choice1Of2
                current |> IVR.map Choice2Of2
            ]

        match r with
        | Choice1Of2 cmd ->
            return! run (Some cmd)
        | Choice2Of2 result ->
            return result
    }

    let post cmd =
        Channel.post (Replace cmd) sender

    post, run None



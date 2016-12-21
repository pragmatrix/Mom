/// A channel offers a way to communicate events across IVRs. The channels are named so that
/// it is possible to create multiple channels of the same type.
module IVR.Channel

open IVR.GlobalExports

type 'message receiver = {
    Name: string
}

type 'message sender = {
    Name: string
}

/// The message that is sent.
type Message<'message> =
    | Message of channel: string * 'message

/// Create a named channel, returns a sender and a receiver pair.
let create<'message>(name) : 'message sender * 'message receiver = 
    { Name = name }, { Name = name }

/// Post a message to a channel.
let post message (channel: 'message sender) = ivr {
    do! IVR.schedule (Message(channel.Name, message))
}

/// Wait for a message on a channel by using selector. Continues waiting if the selector
/// does not match. Note that messages are ignored if a message is sent while the receiver
/// does not wait / selects for it.
let wait' (selector: 'message -> 'r option) (channel: 'message receiver) : 'r ivr =
    IVR.waitFor <|
    fun (Message(name, message)) ->
        if name = channel.Name 
        then selector message
        else None

/// Wait for a message on a channel.
let wait channel = wait' Some channel

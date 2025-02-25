/// A reflector is a place inside the current mom hierarchy that allows nested moms to schedule type
/// safe events to other nested moms.
///
/// A reflector mom provides two control functions to nested moms. One to send an event and one to
/// receive one.
///
/// Semantics:
/// - If a receiver isn't currently listening and the sender is used, the event may be lost.
/// - If a receiver starts listening _right_ after the sender sends out an event (i.e. without any  
///   asynchronous moms running in between), the event will be received.
/// - If the nested mom completes before all events are dispatched, the pending events are lost.
/// - Any number of events can be sent at once. The events are delivered in a FIFO manner.
/// - Multiple reflectors with the same type don't interfere with each other.  
/// - Sending an event is not implement using a Flux request, meaning that it's not observable  
///   (and thus not purely functional). Although this might change later.
[<RequireQualifiedAccess>]
module Mom.Reflector

open Mom.GlobalExports

type Sender<'e> = 'e -> unit mom
type Receiver<'e> = unit -> 'e mom
type Channel<'e> = Sender<'e> * Receiver<'e>

[<AutoOpen>]
module private Private =
    let idGen = Ids.newGenerator()

    [<Struct>]
    type ReflectedEvent<'e> = 
        | ReflectedEvent of Id * 'e

/// Creates a Reflector channel and a next / driver function for the receiving part.
let create() : Channel<'e> =
    // We need a unique id to be sure that this reflector's receiver only process the events send
    // by this reflector's sender.
    let id = idGen.GenerateId()

    // For now, we send the event out _without_ going through a request of the nested Moms. This
    // simplifies testing but reduces transparency. It also allows the event to be sent further up the Mom hierarchy.
    let send (ev: 'e) : unit mom =
        ReflectedEvent(id, ev)
        |> Mom.schedule

    let receive() : 'e mom = 
        Mom.waitFor (function
        | ReflectedEvent(rid, e) when rid = id -> Some(e)
        | _ -> None)

    (send, receive)

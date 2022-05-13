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
open Mom.Flux
open System.Collections.Generic

type Sender<'e> = 'e -> unit mom
type Receiver<'e> = unit -> 'e mom
type Channel<'e> = (Sender<'e> * Receiver<'e>)

[<AutoOpen>]
module private Private =
    let idGen = Ids.newGenerator()

    [<Struct>]
    type ReflectedEvent<'e> = 
        | ReflectedEvent of Id * 'e

/// Wraps a mom so that it can use a direct channel for communication. This is implemented by
/// installing an event reflector at the current node in the execution hierarchy.
let wrap (mkNested: Channel<'e> -> 'a mom) : 'a mom = 
    // We need an unique id to be sure that this reflector's receiver only process the events send
    // by this reflector's sender.
    let id = idGen.GenerateId()
    let queue = Queue<'e>()

    // For now, we send the event out _without_ going through a request of the nested IVR. This
    // simplifies testing but reduces transparency.
    let send (ev: 'e) : unit mom = mom {
        queue.Enqueue(ev)
    }

    let receive() : 'e mom = 
        Mom.Mom.waitFor (function
        | ReflectedEvent(rid, e) when rid = id -> Some(e)
        | _ -> None)

    let rec next = function
        | Requesting(req, cont) -> Requesting(req, cont >> next)
        | Waiting(f) ->
            if queue.Count <> 0 
            then ReflectedEvent(id, queue.Dequeue()) |> f |> next
            else Waiting(f >> next)
        // TODO: might log if not all events are consumed
        | Completed(_) as flux -> flux 
        
    fun () ->
        mkNested (send, receive) () |> next

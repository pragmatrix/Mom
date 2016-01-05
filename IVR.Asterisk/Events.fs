namespace IVR.Asterisk

open IVR

open AsterNET.ARI.Models
open AsterNET.ARI.Actions

module Events = 

    type IEventsCommand<'r> =
        inherit IDispatch<IEventsActions>
        inherit IVR.IReturns<'r>

    type IEventsCommand =
        inherit IDispatch<IEventsActions>

    type EventWebSocket = EventWebSocket of string with
        interface IEventsCommand<Message> with
            member this.dispatch events = 
                let (EventWebSocket application) = this
                events.EventWebsocket(application)
                |> box

    type UserEvent = {
        eventName: string
        application: string
        source: string option
        variables: (string * string) list option
        } with
        interface IEventsCommand with
            member this.dispatch events = 
                events.UserEvent(this.eventName, this.application, opts this.source, optvars this.variables)
                |> box

    [<AbstractClass; Sealed>]
    type Events() =
        static member EventWebSocket(app: string) =
            EventWebSocket app
            |> IVR.send

        static member UserEvent(eventName, application, ?source, ?variables) = 
            { eventName = eventName; application = application; source = source; variables = variables }
            |> IVR.post

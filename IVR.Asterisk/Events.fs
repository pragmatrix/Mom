namespace IVR.Asterisk

open IVR

open AsterNET.ARI.Models

module Events = 

    type EventWebSocket = EventWebSocket of string with
        interface IDispatchAction<Message> with
            member this.dispatch client = 
                let (EventWebSocket application) = this
                client.Events.EventWebsocket(application)
                |> box

    type UserEvent = {
        eventName: string
        application: string
        source: string option
        variables: (string * string) list option
        } with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                client.Events.UserEvent(this.eventName, this.application, opts this.source, optvars this.variables)
                |> box

    [<AbstractClass; Sealed>]
    type Events() =
        static member eventWebSocket(app: string) =
            EventWebSocket app
            |> IVR.send

        static member userEvent(eventName, application, ?source, ?variables) = 
            { eventName = eventName; application = application; source = source; variables = variables }
            |> IVR.post

namespace IVR.Asterisk

open IVR

open AsterNET.ARI.Models

module Applications =

    type List = List with
        interface IDispatchAction<Application list> with
            member this.dispatch client = 
                client.Channels.List() |> Seq.toList |> box

    type Get = Get of applicationName: string with
        interface IDispatchAction<Application> with
            member this.dispatch client =
                let (Get applicationName) = this
                client.Applications.Get(applicationName)
                |> box

    type Subscribe = Subscribe of applicationName: string * eventSource: string with
        interface IDispatchAction<Application> with
            member this.dispatch client =
                let (Subscribe (applicationName, eventSource)) = this
                client.Applications.Subscribe(applicationName, eventSource)
                |> box

    type Unsubscribe = Unsubscribe of applicationName: string * eventSource: string with
        interface IDispatchAction<Application> with
            member this.dispatch client =
                let (Unsubscribe (applicationName, eventSource)) = this
                client.Applications.Unsubscribe(applicationName, eventSource)
                |> box

    
    [<AbstractClass; Sealed>]
    type Applications() =
        static member list() = List |> IVR.send
        static member get(applicationName) = Get applicationName |> IVR.send
        static member subscribe(applicationName: string, eventSource: string) = 
            Subscribe(applicationName, eventSource) |> IVR.send

        static member unsubscribe(applicationName: string, eventSource: string) = 
            Unsubscribe(applicationName, eventSource) |> IVR.send

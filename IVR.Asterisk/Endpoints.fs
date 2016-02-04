namespace IVR.Asterisk

open IVR

open AsterNET.ARI.Models

module Endpoints =

    type List = List with
        interface IDispatchAction<Endpoint list> with
            member this.dispatch client = 
                client.Endpoints.List() |> Seq.toList 
                |> box

    type SendMessage = {
        receiver: string
        sender: string
        body: string option
        variables: (string * string) list option
        } with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                client.Endpoints.SendMessage(
                    this.receiver,
                    this.sender, 
                    this.body |> opts, 
                    this.variables |> optvars)
                |> box

    type ListByTech = ListByTech of tech: string with
        interface IDispatchAction<Endpoint list> with
            member this.dispatch client = 
                let (ListByTech tech) = this
                client.Endpoints.ListByTech(tech) |> Seq.toList
                |> box

    type Get = Get of tech: string * resource: string with
        interface IDispatchAction<Endpoint> with
            member this.dispatch client = 
                let (Get (tech, resource)) = this
                client.Endpoints.Get(tech, resource)
                |> box

    type SendMessageToEndpoint = {
        tech: string
        resource: string
        sender: string
        body: string option
        variables: (string * string) list option
        } with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                client.Endpoints.SendMessageToEndpoint(
                    this.tech,
                    this.resource,
                    this.sender, 
                    this.body |> opts, 
                    this.variables |> optvars)
                |> box


    [<AbstractClass;Sealed>]
    type Endpoints() =
        static member list() =
            List
            |> IVR.send
        static member sendMessage(receiver, sender, ?body, ?variables) =
            { SendMessage.receiver = receiver; sender = sender; body = body; variables = variables }
            |> IVR.post
        static member listByTech(tech) = 
            ListByTech(tech)
            |> IVR.send
        static member get(tech, resource)  = 
            Get(tech, resource)
            |> IVR.send
        static member sendMessageToEndpoint(tech, resource, sender, ?body, ?variables) = 
            { SendMessageToEndpoint.tech = tech; resource = resource; sender = sender; body = body; variables = variables }
            |> IVR.post

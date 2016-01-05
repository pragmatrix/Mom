namespace IVR.Asterisk

open IVR

open AsterNET.ARI.Actions
open AsterNET.ARI.Models

module Endpoints =

    type IEndpointsCommand<'r> =
        inherit IDispatch<IEndpointsActions>
        inherit IVR.IReturns<'r>

    type IEndpointsCommand =
        inherit IDispatch<IEndpointsActions>

    type List = List with
        interface IEndpointsCommand<Endpoint list> with
            member this.dispatch endpoints = 
                endpoints.List() |> Seq.toList 
                |> box

    type SendMessage = {
        receiver: string
        sender: string
        body: string option
        variables: (string * string) list option
        } with
        interface IEndpointsCommand with
            member this.dispatch endpoints = 
                endpoints.SendMessage(
                    this.receiver,
                    this.sender, 
                    this.body |> opts, 
                    this.variables |> optvars)
                |> box

    type ListByTech = ListByTech of tech: string with
        interface IEndpointsCommand<Endpoint list> with
            member this.dispatch endpoints = 
                let (ListByTech tech) = this
                endpoints.ListByTech(tech) |> Seq.toList
                |> box

    type Get = Get of tech: string * resource: string with
        interface IEndpointsCommand<Endpoint> with
            member this.dispatch endpoints = 
                let (Get (tech, resource)) = this
                endpoints.Get(tech, resource)
                |> box

    type SendMessageToEndpoint = {
        tech: string
        resource: string
        sender: string
        body: string option
        variables: (string * string) list option
        } with
        interface IEndpointsCommand with
            member this.dispatch endpoints = 
                endpoints.SendMessageToEndpoint(
                    this.tech,
                    this.resource,
                    this.sender, 
                    this.body |> opts, 
                    this.variables |> optvars)
                |> box


    [<AbstractClass;Sealed>]
    type Endpoints() =
        static member List() =
            List
            |> IVR.send
        static member SendMessage(receiver, sender, ?body, ?variables) =
            { SendMessage.receiver = receiver; sender = sender; body = body; variables = variables }
            |> IVR.post
        static member ListByTech(tech) = 
            ListByTech(tech)
            |> IVR.send
        static member Get(tech, resource)  = 
            Get(tech, resource)
            |> IVR.send
        static member SendMessageToEndpoint(tech, resource, sender, ?body, ?variables) = 
            { SendMessageToEndpoint.tech = tech; resource = resource; sender = sender; body = body; variables = variables }
            |> IVR.post

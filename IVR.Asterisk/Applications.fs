namespace IVR.Asterisk

open IVR

open AsterNET.ARI.Models
open AsterNET.ARI.Actions

module Applications =

    type IApplicationsCommand<'r> =
        inherit IDispatch<IApplicationsActions>
        inherit IVR.IReturns<'r>

    type IApplicationsCommand =
        inherit IDispatch<IApplicationsActions>

    type List = List with
        interface IApplicationsCommand<Application list> with
            member this.dispatch channels = 
                channels.List() |> Seq.toList |> box

    type Get = Get of applicationName: string with
        interface IApplicationsCommand<Application> with
            member this.dispatch applications =
                let (Get applicationName) = this
                applications.Get(applicationName)
                |> box

    type Subscribe = Subscribe of applicationName: string * eventSource: string with
        interface IApplicationsCommand<Application> with
            member this.dispatch applications =
                let (Subscribe (applicationName, eventSource)) = this
                applications.Subscribe(applicationName, eventSource)
                |> box

    type Unsubscribe = Unsubscribe of applicationName: string * eventSource: string with
        interface IApplicationsCommand<Application> with
            member this.dispatch applications =
                let (Unsubscribe (applicationName, eventSource)) = this
                applications.Unsubscribe(applicationName, eventSource)
                |> box

    
    [<AbstractClass; Sealed>]
    type Applications() =
        static member List() = List |> IVR.send
        static member Get(applicationName) = Get applicationName |> IVR.send
        static member Subscribe(applicationName: string, eventSource: string) = 
            Subscribe(applicationName, eventSource) |> IVR.send

        static member Unsubscribe(applicationName: string, eventSource: string) = 
            Unsubscribe(applicationName, eventSource) |> IVR.send

namespace IVR.Asterisk

open IVR

open AsterNET.ARI.Models

module Mailboxes = 

    type List = List with
        interface IDispatchAction<Mailbox list> with
            member this.dispatch client = 
                client.Mailboxes.List() |> Seq.toList 
                |> box

    type Get = Get of mailboxName: string with
        interface IDispatchAction<Mailbox> with
            member this.dispatch client = 
                let (Get mailboxName) = this
                client.Mailboxes.Get(mailboxName)
                |> box

    type Update = {
        mailboxName : string
        oldMessages: int
        newMessages: int
        } with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                client.Mailboxes.Update(this.mailboxName, this.oldMessages, this.newMessages)
                |> box

    type Delete = Delete of mailboxName: string with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (Delete mailboxName) = this
                client.Mailboxes.Delete(mailboxName)
                |> box

    [<AbstractClass; Sealed>]
    type Mailboxes() = 
        static member list() = 
            List
            |> IVR.send
        static member get(mailboxName) = 
            Get(mailboxName)
            |> IVR.send
        static member update(mailboxName, oldMessages, newMessages) = 
            { mailboxName = mailboxName; oldMessages = oldMessages; newMessages = newMessages }
            |> IVR.post

    



        

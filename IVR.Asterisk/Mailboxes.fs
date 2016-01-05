namespace IVR.Asterisk

open IVR

open AsterNET.ARI.Models
open AsterNET.ARI.Actions

module Mailboxes = 
    
    type IMailboxesCommand<'r> =
        inherit IDispatch<IMailboxesActions>
        inherit IVR.IReturns<'r>

    type IMailboxesCommand =
        inherit IDispatch<IMailboxesActions>

    type List = List with
        interface IMailboxesCommand<Mailbox list> with
            member this.dispatch mailboxes = 
                mailboxes.List() |> Seq.toList 
                |> box

    type Get = Get of mailboxName: string with
        interface IMailboxesCommand<Mailbox> with
            member this.dispatch mailboxes = 
                let (Get mailboxName) = this
                mailboxes.Get(mailboxName)
                |> box

    type Update = {
        mailboxName : string
        oldMessages: int
        newMessages: int
        } with
        interface IMailboxesCommand with
            member this.dispatch mailboxes = 
                mailboxes.Update(this.mailboxName, this.oldMessages, this.newMessages)
                |> box

    type Delete = Delete of mailboxName: string with
        interface IMailboxesCommand with
            member this.dispatch mailboxes = 
                let (Delete mailboxName) = this
                mailboxes.Delete(mailboxName)
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

    



        

namespace IVR.Asterisk

open IVR

open AsterNET.ARI.Models

module Sounds =

    type List = List of lang: string option * format: string option with
        interface IDispatchAction<Sound list> with
            member this.dispatch client = 
                let (List (lang, format)) = this
                client.Sounds.List(opts lang, opts format) |> Seq.toList
                |> box

    type Get = Get of soundId: string with
        interface IDispatchAction<Sound> with
            member this.dispatch client = 
                let (Get soundId) = this
                client.Sounds.Get(soundId)
                |> box

    [<AbstractClass; Sealed>]
    type Sounds() = 
        member this.list(?lang, ?format) = 
            List(lang, format)
            |> IVR.send
        member this.get(soundId) = 
            Get(soundId)
            |> IVR.send
        
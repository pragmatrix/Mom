namespace IVR.Asterisk

open IVR

open AsterNET.ARI.Models
open AsterNET.ARI.Actions

module Sounds =

    type ISoundsCommand<'r> =
        inherit IDispatch<ISoundsActions>
        inherit IVR.IReturns<'r>

    type ISoundsCommand =
        inherit IDispatch<ISoundsActions>

    type List = List of lang: string option * format: string option with
        interface ISoundsCommand<Sound list> with
            member this.dispatch sounds = 
                let (List (lang, format)) = this
                sounds.List(opts lang, opts format) |> Seq.toList
                |> box

    type Get = Get of soundId: string with
        interface ISoundsCommand<Sound> with
            member this.dispatch sounds = 
                let (Get soundId) = this
                sounds.Get(soundId)
                |> box

    [<AbstractClass; Sealed>]
    type Sounds() = 
        member this.List(?lang, ?format) = 
            List(lang, format)
            |> IVR.send
        member this.Get(soundId) = 
            Get(soundId)
            |> IVR.send
        
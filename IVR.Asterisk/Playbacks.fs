namespace IVR.Asterisk

open System
open System.Collections.Generic

open IVR

open AsterNET.ARI.Models
open AsterNET.ARI.Actions


module Playbacks =

    type IPlaybacksCommand<'r> =
        inherit IDispatch<IPlaybacksActions>
        inherit IVR.IReturns<'r>

    type IPlaybacksCommand =
        inherit IDispatch<IPlaybacksActions>

    type Get = Get of playbackId: string with
        interface IPlaybacksCommand<Playback> with
            member this.dispatch playbacks = 
                let (Get playbackId) = this
                playbacks.Get(playbackId)
                |> box

    type Stop = Stop of playbackId: string with
        interface IPlaybacksCommand<Playback> with
            member this.dispatch playbacks = 
                let (Stop playbackId) = this
                playbacks.Stop(playbackId)
                |> box

    type Operation =
        | Restart = 0
        | Pause = 1
        | Unpause = 2
        | Reverse = 3
        | Forward = 4

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Operation =
        let tos = 
            function
            | Operation.Restart -> "restart"
            | Operation.Pause -> "pause"
            | Operation.Unpause -> "unpause"
            | Operation.Reverse -> "reverse"
            | Operation.Forward -> "forward"
            | v -> failwithf "invalid Operation: %A" v

    type Control = Control of playbackId: string * operation: Operation with
        interface IPlaybacksCommand with
            member this.dispatch playbacks = 
                let (Control (playbackId, operation)) = this
                playbacks.Control(playbackId, operation |> Operation.tos)
                |> box

    [<AbstractClass; Sealed>]
    type Playbacks() = 
        static member Get(playbackId) = 
            Get playbackId
        static member Stop(playbackId) = 
            Stop playbackId
        static member Control(playbackId, operation) = 
            Control(playbackId, operation)




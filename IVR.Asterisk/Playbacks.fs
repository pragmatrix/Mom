namespace IVR.Asterisk

open System
open System.Collections.Generic

open IVR

open AsterNET.ARI.Models
open AsterNET.ARI.Actions

module Playbacks =

    type Get = Get of playbackId: string with
        interface IDispatchAction<Playback> with
            member this.dispatch client = 
                let (Get playbackId) = this
                client.Playbacks.Get(playbackId)
                |> box

    type Stop = Stop of playbackId: string with
        interface IDispatchAction<Playback> with
            member this.dispatch client = 
                let (Stop playbackId) = this
                client.Playbacks.Stop(playbackId)
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
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (Control (playbackId, operation)) = this
                client.Playbacks.Control(playbackId, operation |> Operation.tos)
                |> box

    [<AbstractClass; Sealed>]
    type Playbacks() = 
        static member get(playbackId) = 
            Get playbackId
            |> IVR.send
        static member stop(playbackId) = 
            Stop playbackId
            |> IVR.send
        static member control(playbackId, operation) = 
            Control(playbackId, operation)
            |> IVR.post




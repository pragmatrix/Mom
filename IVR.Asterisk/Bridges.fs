namespace IVR.Asterisk

open System

open IVR

open AsterNET.ARI.Models
open AsterNET.ARI.Actions

module Bridges =
    
    type IBridgesCommand<'r> =
        inherit IDispatch<IBridgesActions>
        inherit IVR.IReturns<'r>

    type IBridgesCommand =
        inherit IDispatch<IBridgesActions>

    type List = List with
        interface IBridgesCommand<Bridge list> with
            member this.dispatch bridges = 
                bridges.List() |> Seq.toList
                |> box

    type TypeAttribute = 
        | Mixing = 0
        | Holding = 1
        | DTMFEvents = 2
        | ProxyMedia = 3

    [<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
    module TypeAttribute = 
        let tos = 
            function 
            | TypeAttribute.Mixing -> "mixing"
            | TypeAttribute.Holding -> "holding"
            | TypeAttribute.DTMFEvents -> "dtmf_events"
            | TypeAttribute.ProxyMedia -> "proxy_media"
            | ta -> failwithf "invalid type attribute: %A" ta

    type Create = {
        typeAttributes: TypeAttribute list option
        bridgeId: string option
        name: string option
        } with
        interface IBridgesCommand<Bridge> with
            member this.dispatch bridges = 
                bridges.Create(
                    this.typeAttributes |> Option.map (List.map TypeAttribute.tos >> css) |> opts,
                    this.bridgeId |> opts,
                    this.name |> opts)
                |> box

    type Get = Get of bridgeId: string with
        interface IBridgesCommand<Bridge> with
            member this.dispatch bridges = 
                let (Get bridgeId) = this
                bridges.Get(bridgeId)
                |> box

    type Destroy = Destroy of bridgeId: string with
        interface IBridgesCommand with
            member this.dispatch bridges = 
                let (Destroy bridgeId) = this
                bridges.Destroy(bridgeId)
                |> box

    type AddChannel = {
        bridgeId: string
        channels: string list
        role: string option
        } with
        interface IBridgesCommand with
            member this.dispatch bridges = 
                bridges.AddChannel(
                    this.bridgeId,
                    this.channels |> css,
                    this.role |> opts)
                |> box

    type RemoveChannel = {
        bridgeId: string
        channels: string list
        } with
        interface IBridgesCommand with
            member this.dispatch bridges = 
                bridges.RemoveChannel(
                    this.bridgeId,
                    this.channels |> css)
                |> box

    type StartMOH = StartMOH of bridgeId: string * mohClass: string option with
        interface IBridgesCommand with
            member this.dispatch bridges = 
                let (StartMOH (bridgeId, mohClass)) = this
                bridges.StartMoh(bridgeId, mohClass |> opts)
                |> box

    type StopMOH = StopMOH of bridgeId: string with
        interface IBridgesCommand with
            member this.dispatch bridges = 
                let (StopMOH bridgeId) = this
                bridges.StopMoh(bridgeId)
                |> box

    [<NoComparison>]
    type Play = {
        bridgeId: string
        media: Uri
        lang: string option
        offset: TimeSpan option
        skip: TimeSpan option
        playbackId: string option
        } with 
        interface IBridgesCommand<Playback> with
            member this.dispatch bridges = 
                bridges.Play(
                    this.bridgeId,
                    this.media.ToString(),
                    opts this.lang,
                    this.offset |> Option.map TimeSpan.toms |> opt,
                    this.skip |> Option.map TimeSpan.toms |> opt,
                    opts this.playbackId) 
                |> box

    type Record = {
        bridgeId: string
        name: string
        format: string
        maxDuration: TimeSpan option
        maxSilence: TimeSpan option
        ifExists: IfExists option
        beep: bool option
        terminateOn: TerminateOn option
        } with 
        interface IBridgesCommand<LiveRecording> with
            member this.dispatch channels = 
                channels.Record(
                    this.bridgeId,
                    this.name,
                    this.format,
                    this.maxDuration |> Option.map TimeSpan.tosec |> opt,
                    this.maxSilence |> Option.map TimeSpan.tosec |> opt,
                    this.ifExists |> Option.map IfExists.tos |> opts,
                    opt this.beep,
                    this.terminateOn |> Option.map TerminateOn.tos |> opts)
               |> box

    [<AbstractClass;Sealed>]
    type Bridges() =
        static member list() = 
            List
            |> IVR.send
        static member create(?typeAttributes, ?bridgeId, ?name) = 
            { Create.typeAttributes = typeAttributes; bridgeId = bridgeId; name = name }
            |> IVR.send
        static member get(bridgeId) = 
            Get(bridgeId)
            |> IVR.send
        static member destroy(bridgeId) = 
            Destroy(bridgeId)
            |> IVR.post
        static member addChannel(bridgeId, channels, ?role) =
            { AddChannel.bridgeId = bridgeId; channels = channels; role = role }
            |> IVR.post
        static member removeChannel(bridgeId, channels) = 
            { RemoveChannel.bridgeId = bridgeId; channels = channels }
            |> IVR.post
        static member startMOH(bridgeId, ?mohClass) =
            StartMOH(bridgeId, mohClass)
            |> IVR.post
        static member stopMOH(bridgeId) = 
            StopMOH(bridgeId)
            |> IVR.post
        static member play(bridgeId, media, ?lang, ?offset, ?skip, ?playbackId) =
            {
                Play.bridgeId = bridgeId
                media = media
                lang = lang
                offset = offset
                skip = skip
                playbackId = playbackId
            }
            |> IVR.send
        static member record(bridgeId, name, format, ?maxDuration, ?maxSilence, ?ifExists, ?beep, ?terminateOn) =
            {
                Record.bridgeId = bridgeId
                name = name
                format = format
                maxDuration = maxDuration
                maxSilence = maxSilence
                ifExists = ifExists
                beep = beep
                terminateOn = terminateOn
            }
            |> IVR.send

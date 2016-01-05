namespace IVR.Asterisk

open System

open IVR

open AsterNET.ARI.Models
open AsterNET.ARI.Actions

module Channels =

    type IChannelsCommand<'r> =
        inherit IDispatch<IChannelsActions>
        inherit IVR.IReturns<'r>

    type IChannelsCommand =
        inherit IDispatch<IChannelsActions>

    type HangupReason =
        | Normal = 0
        | Busy = 1
        | Congestion = 2
        | NoAnswer = 3

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module HangupReason =
        let tos = 
            function
            | HangupReason.Normal -> "normal"
            | HangupReason.Busy -> "busy"
            | HangupReason.Congestion -> "congestion"
            | HangupReason.NoAnswer -> "no_answer"
            | v -> failwithf "invalid HangupReason: %A" v

    type MuteDirection = 
        | Both = 0
        | In = 1
        | Out = 2

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MuteDirection = 
        let tos =
            function
            | MuteDirection.In -> "in"
            | MuteDirection.Out -> "out"
            | MuteDirection.Both -> "both"
            | v -> failwithf "invalid MuteDirection: %A" v

    type Timeout = 
        | Timeout of TimeSpan
        | NoTimeout
        with
        static member toi = 
            function 
            | Timeout ts -> ts.TotalSeconds |> int
            | NoTimeout -> -1

    type AudioDirection = 
        | None = 0
        | Both = 1
        | Out = 2
        | In = 3

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AudioDirection = 
        let tos = 
            function 
            | AudioDirection.None -> "none"
            | AudioDirection.Both -> "both"
            | AudioDirection.Out -> "out"
            | AudioDirection.In -> "in"
            | v -> failwithf "invalid AudioDirection: %A" v

    type List = List with
        interface IChannelsCommand<Channel list> with
            member this.dispatch channels = 
                channels.List() |> Seq.toList 
                |> box

    type Originate = { 
        endpoint:string
        extension: string option 
        context: string option
        priority: int64 option
        label: string option
        app: string option
        appArgs: string option
        callerId: string option
        timeout: Timeout option
        variables: (string * string) list option
        channelId: string option
        otherChannelId: string option
        originator: string option
        } with 
        interface IChannelsCommand<Channel> with
            member this.dispatch channels = 
                channels.Originate(
                    this.endpoint, 
                    opts this.extension, 
                    opts this.context, 
                    opt this.priority, 
                    opts this.label, 
                    opts this.app, 
                    opts this.appArgs, 
                    opts this.callerId, 
                    this.timeout |> Option.map Timeout.toi |> opt, 
                    optvars this.variables,
                    opts this.channelId, 
                    opts this.otherChannelId, 
                    opts this.originator) 
                |> box

    type Get = Get of channelId: string with 
        interface IChannelsCommand<Channel> with
            member this.dispatch channels = 
                let (Get channelId) = this
                channels.Get(channelId)
                |> box

    type Hangup = Hangup of channelId: string * reason: HangupReason option 
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (Hangup (channelId, reason)) = this
                let reason = reason |> Option.map HangupReason.tos
                channels.Hangup(channelId, opts reason)
                |> box

    type ContinueInDialplan = {
        channelId: string
        context: string option
        extension: string option
        priority: int option
        label: string option
        } with
        interface IChannelsCommand with
            member this.dispatch channels = 
                channels.ContinueInDialplan(
                    this.channelId,
                    opts this.context,
                    opts this.extension,
                    opt this.priority,
                    opts this.label)
                |> box

    type Redirect = Redirect of channelId: string * endpoint: string
        with
        interface IChannelsCommand with
            member this.dispatch channels =     
                let (Redirect (channelId, endpoint)) = this
                channels.Redirect(channelId, endpoint) 
                |> box

    type Answer = Answer of channelId: string
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (Answer channelId) = this
                channels.Answer(channelId) 
                |> box

    type Ring = Ring of channelId: string
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (Ring channelId) = this
                channels.Ring(channelId) 
                |> box

    type RingStop = RingStop of channelId: string
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (RingStop channelId) = this
                channels.RingStop(channelId) 
                |> box

    type SendDTMF = {
        channelId: string
        dtmf: string
        before: TimeSpan option
        between: TimeSpan option
        duration: TimeSpan option
        after: TimeSpan option
        } with
        interface IChannelsCommand with
            member this.dispatch channels = 
                channels.SendDTMF(
                    this.channelId,
                    this.dtmf,
                    this.before |> Option.map TimeSpan.toms |> opt,
                    this.between |> Option.map TimeSpan.toms |> opt,
                    this.duration |> Option.map TimeSpan.toms |> opt,
                    this.after |> Option.map TimeSpan.toms |> opt) 
                |> box

    type Mute = Mute of channelId: string * direction: MuteDirection option
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (Mute (channelId, direction)) = this
                channels.Mute(channelId, direction |> Option.map MuteDirection.tos |> opts) 
                |> box

    type Unmute = Unmute of channelId: string * direction: MuteDirection option
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (Unmute (channelId, direction)) = this
                channels.Unmute(channelId, direction |> Option.map MuteDirection.tos |> opts) 
                |> box

    type Hold = Hold of channelId: string
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (Hold channelId) = this
                channels.Hold(channelId) 
                |> box

    type Unhold = Unhold of channelId: string
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (Unhold channelId) = this
                channels.Unhold(channelId) 
                |> box

    type StartMOH = StartMOH of channelId: string * mohClass: string option
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (StartMOH (channelId, mohClass)) = this
                channels.StartMoh(channelId, opts mohClass) 
                |> box

    type StopMOH = StopMOH of channelId: string
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (StopMOH channelId) = this
                channels.StopMoh(channelId) 
                |> box

    type StartSilence = StartSilence of channelId: string
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (StartSilence channelId) = this
                channels.StartSilence(channelId) 
                |> box

    type StopSilence = StopSilence of channelId: string
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (StopSilence channelId) = this
                channels.StopSilence(channelId) 
                |> box

    type Play = {
        channelId: string
        media: Uri
        lang: string option
        offset: TimeSpan option
        skip: TimeSpan option
        playbackId: string option
        } with 
        interface IChannelsCommand<Playback> with
            member this.dispatch channels = 
                channels.Play(
                    this.channelId,
                    this.media.ToString(),
                    opts this.lang,
                    this.offset |> Option.map TimeSpan.toms |> opt,
                    this.skip |> Option.map TimeSpan.toms |> opt,
                    opts this.playbackId) 
                |> box

    type Record = {
        channelId: string
        name: string
        format: string
        maxDuration: TimeSpan option
        maxSilence: TimeSpan option
        ifExists: IfExists option
        beep: bool option
        terminateOn: TerminateOn option
        } with 
        interface IChannelsCommand<LiveRecording> with
            member this.dispatch channels = 
                channels.Record(
                    this.channelId,
                    this.name,
                    this.format,
                    this.maxDuration |> Option.map TimeSpan.tosec |> opt,
                    this.maxSilence |> Option.map TimeSpan.tosec |> opt,
                    this.ifExists |> Option.map IfExists.tos |> opts,
                    opt this.beep,
                    this.terminateOn |> Option.map TerminateOn.tos |> opts)
               |> box

    type GetChannelVar = GetChannelVar of channelId: string * variable: string
        with
        interface IChannelsCommand<Variable> with
            member this.dispatch channels = 
                let (GetChannelVar (channelId, variable)) = this
                channels.GetChannelVar(channelId, variable) |> box
    
    type SetChannelVar = SetChannelVar of channelId: string * variable: string * value: string option
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (SetChannelVar (channelId, variable, value)) = this
                channels.SetChannelVar(channelId, variable, opts value)
                |> box

    type SnoopChannel = {
        channelId: string
        app: string
        spy: AudioDirection option
        whisper: AudioDirection option
        appArgs: string option
        snoopId: string option 
        } with
        interface IChannelsCommand<Channel> with
            member this.dispatch channels = 
                channels.SnoopChannel(
                    this.channelId,
                    this.app,
                    this.spy |> Option.map AudioDirection.tos |> opts,
                    this.whisper |> Option.map AudioDirection.tos |> opts,
                    opts this.appArgs,
                    opts this.snoopId) 
                |> box
    
    [<AbstractClass; Sealed>]
    type Channels() = 
        static member List() =
            List
            |> IVR.send
        static member Originate(endpoint, ?extension, ?context, ?priority, ?label, ?app, ?appArgs, ?callerId, ?timeout, ?variables, ?channelId, ?otherChannelId, ?originator) =
            {
                endpoint = endpoint
                extension = extension
                context = context
                priority = priority
                label = label
                app = app
                appArgs = appArgs
                callerId = callerId
                timeout = timeout
                variables = variables
                channelId = channelId
                otherChannelId = otherChannelId
                originator = originator
            }
            |> IVR.send
        static member Get(channelId) = 
            Get channelId
            |> IVR.send
        static member Hangup(channelId, ?reason) = 
            Hangup (channelId, ?reason = reason)
            |> IVR.post
        static member ContinueInDialplan(channelId, ?context, ?extension, ?priority, ?label) =
            {
                ContinueInDialplan.channelId = channelId
                context = context
                extension = extension
                priority = priority
                label = label
            }
            |> IVR.post
        static member Redirect(channelId, endpoint) = 
            Redirect (channelId, endpoint)
            |> IVR.post
        static member Answer(channelId) =
            Answer channelId
            |> IVR.post
        static member Ring(channelId) = 
            Ring channelId
            |> IVR.post
        static member RingStop(channelId) = 
            RingStop channelId
            |> IVR.post
        static member SendDTMF(channelId, dtmf, ?before, ?between, ?duration, ?after) =
            {
                SendDTMF.channelId = channelId
                dtmf = dtmf
                before = before
                between = between
                duration = duration
                after = after
            }
            |> IVR.post
        static member Mute(channelId, ?direction) =
            Mute (channelId, direction)
            |> IVR.post
        static member Unmute(channelId, ?direction) = 
            Unmute (channelId, direction)
            |> IVR.post
        static member Hold(channelId) = 
            Hold channelId
            |> IVR.post
        static member Unhold(channelId) = 
            Unhold channelId
            |> IVR.post
        static member StartMOH(channelId, ?mohClass) =
            StartMOH (channelId, mohClass)
            |> IVR.post
        static member StopMOH(channelId) =
            StopMOH channelId
            |> IVR.post

        static member StartSilence(channelId) = 
            StartSilence channelId
            |> IVR.post
        static member StopSilence(channelId) =
            StopSilence channelId
            |> IVR.post

        static member Play(channelId, media, ?lang, ?offset, ?skip, ?playbackId) =
            {
                Play.channelId = channelId
                media = media
                lang = lang
                offset = offset
                skip = skip
                playbackId = playbackId
            }
            |> IVR.send
        static member Record(channelId, name, format, ?maxDuration, ?maxSilence, ?ifExists, ?beep, ?terminateOn) =
            {
                Record.channelId = channelId
                name = name
                format = format
                maxDuration = maxDuration
                maxSilence = maxSilence
                ifExists = ifExists
                beep = beep
                terminateOn = terminateOn
            }
            |> IVR.send
        static member GetChannelVar(channelId, variable) =
            GetChannelVar (channelId, variable)
            |> IVR.send
        static member SetChannelVar(channelId, variable, ?value) = 
            SetChannelVar (channelId, variable, value)
            |> IVR.post
        static member SnoopChannel(channelId, app, ?spy, ?whisper, ?appArgs, ?snoopId) = 
            {
                SnoopChannel.channelId = channelId
                app = app
                spy = spy
                whisper = whisper
                appArgs = appArgs
                snoopId = snoopId
            }
            |> IVR.send


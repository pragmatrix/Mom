namespace IVR.Asterisk

open System

open IVR

open AsterNET.ARI.Models

module Channels =

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
        interface IDispatchAction<Channel list> with
            member this.dispatch client = 
                client.Channels.List() |> Seq.toList 
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
        interface IDispatchAction<Channel> with
            member this.dispatch client = 
                client.Channels.Originate(
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
        interface IDispatchAction<Channel> with
            member this.dispatch client = 
                let (Get channelId) = this
                client.Channels.Get(channelId)
                |> box

    type Hangup = Hangup of channelId: string * reason: HangupReason option 
        with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (Hangup (channelId, reason)) = this
                let reason = reason |> Option.map HangupReason.tos
                client.Channels.Hangup(channelId, opts reason)
                |> box

    type ContinueInDialplan = {
        channelId: string
        context: string option
        extension: string option
        priority: int option
        label: string option
        } with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                client.Channels.ContinueInDialplan(
                    this.channelId,
                    opts this.context,
                    opts this.extension,
                    opt this.priority,
                    opts this.label)
                |> box

    type Redirect = Redirect of channelId: string * endpoint: string
        with
        interface IDispatchAction<unit> with
            member this.dispatch client =     
                let (Redirect (channelId, endpoint)) = this
                client.Channels.Redirect(channelId, endpoint) 
                |> box

    type Answer = Answer of channelId: string
        with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (Answer channelId) = this
                client.Channels.Answer(channelId) 
                |> box

    type Ring = Ring of channelId: string
        with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (Ring channelId) = this
                client.Channels.Ring(channelId) 
                |> box

    type RingStop = RingStop of channelId: string
        with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (RingStop channelId) = this
                client.Channels.RingStop(channelId) 
                |> box

    type SendDTMF = {
        channelId: string
        dtmf: string
        before: TimeSpan option
        between: TimeSpan option
        duration: TimeSpan option
        after: TimeSpan option
        } with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                client.Channels.SendDTMF(
                    this.channelId,
                    this.dtmf,
                    this.before |> Option.map TimeSpan.toms |> opt,
                    this.between |> Option.map TimeSpan.toms |> opt,
                    this.duration |> Option.map TimeSpan.toms |> opt,
                    this.after |> Option.map TimeSpan.toms |> opt) 
                |> box

    type Mute = Mute of channelId: string * direction: MuteDirection option
        with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (Mute (channelId, direction)) = this
                client.Channels.Mute(channelId, direction |> Option.map MuteDirection.tos |> opts) 
                |> box

    type Unmute = Unmute of channelId: string * direction: MuteDirection option
        with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (Unmute (channelId, direction)) = this
                client.Channels.Unmute(channelId, direction |> Option.map MuteDirection.tos |> opts) 
                |> box

    type Hold = Hold of channelId: string
        with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (Hold channelId) = this
                client.Channels.Hold(channelId) 
                |> box

    type Unhold = Unhold of channelId: string
        with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (Unhold channelId) = this
                client.Channels.Unhold(channelId) 
                |> box

    type StartMOH = StartMOH of channelId: string * mohClass: string option
        with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (StartMOH (channelId, mohClass)) = this
                client.Channels.StartMoh(channelId, opts mohClass) 
                |> box

    type StopMOH = StopMOH of channelId: string
        with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (StopMOH channelId) = this
                client.Channels.StopMoh(channelId) 
                |> box

    type StartSilence = StartSilence of channelId: string
        with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (StartSilence channelId) = this
                client.Channels.StartSilence(channelId) 
                |> box

    type StopSilence = StopSilence of channelId: string
        with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (StopSilence channelId) = this
                client.Channels.StopSilence(channelId) 
                |> box

    [<NoComparison>]
    type Play = {
        channelId: string
        media: Uri
        lang: string option
        offset: TimeSpan option
        skip: TimeSpan option
        playbackId: string option
        } with 
        interface IDispatchAction<Playback> with
            member this.dispatch client = 
                client.Channels.Play(
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
        interface IDispatchAction<LiveRecording> with
            member this.dispatch client = 
                client.Channels.Record(
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
        interface IDispatchAction<Variable> with
            member this.dispatch client = 
                let (GetChannelVar (channelId, variable)) = this
                client.Channels.GetChannelVar(channelId, variable) |> box
    
    type SetChannelVar = SetChannelVar of channelId: string * variable: string * value: string option
        with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (SetChannelVar (channelId, variable, value)) = this
                client.Channels.SetChannelVar(channelId, variable, opts value)
                |> box

    type SnoopChannel = {
        channelId: string
        app: string
        spy: AudioDirection option
        whisper: AudioDirection option
        appArgs: string option
        snoopId: string option 
        } with
        interface IDispatchAction<Channel> with
            member this.dispatch client = 
                client.Channels.SnoopChannel(
                    this.channelId,
                    this.app,
                    this.spy |> Option.map AudioDirection.tos |> opts,
                    this.whisper |> Option.map AudioDirection.tos |> opts,
                    opts this.appArgs,
                    opts this.snoopId) 
                |> box
    
    [<AbstractClass; Sealed>]
    type Channels() = 
        static member list() =
            List
            |> IVR.send
        static member originate(endpoint, ?extension, ?context, ?priority, ?label, ?app, ?appArgs, ?callerId, ?timeout, ?variables, ?channelId, ?otherChannelId, ?originator) =
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
        static member get(channelId) = 
            Get channelId
            |> IVR.send
        static member hangup(channelId, ?reason) = 
            Hangup (channelId, ?reason = reason)
            |> IVR.post
        static member continueInDialplan(channelId, ?context, ?extension, ?priority, ?label) =
            {
                ContinueInDialplan.channelId = channelId
                context = context
                extension = extension
                priority = priority
                label = label
            }
            |> IVR.post
        static member redirect(channelId, endpoint) = 
            Redirect (channelId, endpoint)
            |> IVR.post
        static member answer(channelId) =
            Answer channelId
            |> IVR.post
        static member ring(channelId) = 
            Ring channelId
            |> IVR.post
        static member ringStop(channelId) = 
            RingStop channelId
            |> IVR.post
        static member sendDTMF(channelId, dtmf, ?before, ?between, ?duration, ?after) =
            {
                SendDTMF.channelId = channelId
                dtmf = dtmf
                before = before
                between = between
                duration = duration
                after = after
            }
            |> IVR.post
        static member mute(channelId, ?direction) =
            Mute (channelId, direction)
            |> IVR.post
        static member unmute(channelId, ?direction) = 
            Unmute (channelId, direction)
            |> IVR.post
        static member hold(channelId) = 
            Hold channelId
            |> IVR.post
        static member unhold(channelId) = 
            Unhold channelId
            |> IVR.post
        static member startMOH(channelId, ?mohClass) =
            StartMOH (channelId, mohClass)
            |> IVR.post
        static member stopMOH(channelId) =
            StopMOH channelId
            |> IVR.post

        static member startSilence(channelId) = 
            StartSilence channelId
            |> IVR.post
        static member stopSilence(channelId) =
            StopSilence channelId
            |> IVR.post

        static member play(channelId, media, ?lang, ?offset, ?skip, ?playbackId) =
            {
                Play.channelId = channelId
                media = media
                lang = lang
                offset = offset
                skip = skip
                playbackId = playbackId
            }
            |> IVR.send
        static member record(channelId, name, format, ?maxDuration, ?maxSilence, ?ifExists, ?beep, ?terminateOn) =
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
        static member getChannelVar(channelId, variable) =
            GetChannelVar (channelId, variable)
            |> IVR.send
        static member getChannelVar(channelId, variable, ?value) = 
            SetChannelVar (channelId, variable, value)
            |> IVR.post
        static member snoopChannel(channelId, app, ?spy, ?whisper, ?appArgs, ?snoopId) = 
            {
                SnoopChannel.channelId = channelId
                app = app
                spy = spy
                whisper = whisper
                appArgs = appArgs
                snoopId = snoopId
            }
            |> IVR.send


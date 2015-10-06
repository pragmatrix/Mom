namespace IVR.Asterisk

open System
open AsterNET.ARI
open AsterNET.ARI.Middleware
open AsterNET.ARI.Models
open AsterNET.ARI.Actions
open IVR
open System.Collections.Generic

[<AutoOpen>]
module private Helper =
    let inline opts (o: string option) = 
        match o with
        | Some s -> s
        | None -> null

    let inline opt (i: 'i option) = 
        match i with
        | Some i -> Nullable(i)
        | None -> Nullable()

    let inline optvars (i: ('a * 'b) list option ) = 
        match i with
        | None -> null
        | Some l -> l |> List.map KeyValuePair |> List

module Commands = 

    open IVR

    type IDispatch<'i> =
        abstract member dispatch : 'i -> Response

    type IChannelsCommand<'r> =
        inherit IDispatch<IChannelsActions>
        inherit IVR.IReturns<'r>

    type IChannelsCommand =
        inherit IDispatch<IChannelsActions>

    type List = List with
        interface IChannelsCommand<Channel list> with
            member this.dispatch channels = 
                channels.List() |> Seq.toList |> box

    type Originate = { 
        endpoint:string
        extension: string option 
        context: string option
        priority: int64 option
        label: string option
        app: string option
        appArgs: string option
        callerId: string option
        timeout: int option
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
                    opt this.timeout, 
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

    type Hangup = Hangup of channelId: string * reason: string option 
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (Hangup (channelId, reason)) = this
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
                channels.Redirect(channelId, endpoint) |> box

    type Answer = Answer of channelId: string
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (Answer channelId) = this
                channels.Answer(channelId) |> box

    type Ring = Ring of channelId: string
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (Ring channelId) = this
                channels.Ring(channelId) |> box

    type RingStop = RingStop of channelId: string
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (RingStop channelId) = this
                channels.RingStop(channelId) |> box

    type SendDTMF = {
        channelId: string
        dtmf: string
        before: int option
        between: int option
        duration: int option
        after: int option
        } with
        interface IChannelsCommand with
            member this.dispatch channels = 
                channels.SendDTMF(
                    this.channelId,
                    this.dtmf,
                    opt this.before,
                    opt this.between,
                    opt this.duration,
                    opt this.after) |> box

    type Mute = Mute of channelId: string * direction: string option
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (Mute (channelId, direction)) = this
                channels.Mute(channelId, opts direction) |> box

    type Unmute = Unmute of channelId: string * direction: string option
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (Unmute (channelId, direction)) = this
                channels.Unmute(channelId, opts direction) |> box

    type Hold = Hold of channelId: string
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (Hold channelId) = this
                channels.Hold(channelId) |> box

    type Unhold = Unhold of channelId: string
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (Unhold channelId) = this
                channels.Unhold(channelId) |> box

    type StartMOH = StartMOH of channelId: string * mohClass: string option
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (StartMOH (channelId, mohClass)) = this
                channels.StartMoh(channelId, opts mohClass) |> box

    type StopMOH = StopMOH of channelId: string
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (StopMOH channelId) = this
                channels.StopMoh(channelId) |> box

    type StartSilence = StartSilence of channelId: string
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (StartSilence channelId) = this
                channels.StartSilence(channelId) |> box

    type StopSilence = StopSilence of channelId: string
        with
        interface IChannelsCommand with
            member this.dispatch channels = 
                let (StopSilence channelId) = this
                channels.StopSilence(channelId) |> box

    type Play = {
        channelId: string
        media: string
        lang: string option
        offsetms: int option
        skipms: int option
        playbackId: string option
        } with 
        interface IChannelsCommand<Playback> with
            member this.dispatch channels = 
                channels.Play(
                    this.channelId,
                    this.media,
                    opts this.lang,
                    opt this.offsetms,
                    opt this.skipms,
                    opts this.playbackId) |> box

    type Record = {
        channelId: string
        name: string
        format: string
        maxDurationSeconds: int option
        maxSilenceSeconds: int option
        ifExists: string option
        beep: bool option
        terminateOn: string option
        } with 
        interface IChannelsCommand<LiveRecording> with
            member this.dispatch channels = 
                channels.Record(
                    this.channelId,
                    this.name,
                    this.format,
                    opt this.maxDurationSeconds,
                    opt this.maxSilenceSeconds,
                    opts this.ifExists,
                    opt this.beep,
                    opts this.terminateOn) |> box

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
                channels.SetChannelVar(channelId, variable, opts value) |> box

    type SnoopChannel = {
        channelId: string
        app: string
        spy: string option
        whisper: string option
        appArgs: string option
        snoopId: string option 
        } with
        interface IChannelsCommand<Channel> with
            member this.dispatch channels = 
                channels.SnoopChannel(
                    this.channelId,
                    this.app,
                    opts this.spy,
                    opts this.whisper,
                    opts this.appArgs,
                    opts this.snoopId) |> box
    
    // Do we really need this here anymore, almost all commands are realized as extension methods to the Channel instance.

    [<AbstractClass; Sealed>]
    type Channels() = 
        static member List() = List
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
        static member Get(channelId) = Get channelId
        static member Hangup(channelId, ?reason) = 
            Hangup (channelId, ?reason = reason)
        static member OriginateWithId(endpoint, channelId, ?extension, ?context, ?priority, ?label, ?app, ?appArgs, ?callerId, ?timeout, ?variables, ?otherChannelId, ?originator) =
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
        static member ContinueInDialplan(channelId, ?context, ?extension, ?priority, ?label) =
            {
                ContinueInDialplan.channelId = channelId
                context = context
                extension = extension
                priority = priority
                label = label
            }
        static member Redirect(channelId, endpoint) = 
            Redirect (channelId, endpoint)
        static member Answer(channelId) =
            Answer channelId
        static member Ring(channelId) = 
            Ring channelId
        static member RingStop(channelId) = 
            RingStop channelId
        static member SendDTMF(channelId, dtmf, ?before, ?between, ?duration, ?after) =
            {
                SendDTMF.channelId = channelId
                dtmf = dtmf
                before = before
                between = between
                duration = duration
                after = after
            }
        static member Mute(channelId, ?direction) =
            Mute (channelId, direction)
        static member Unmute(channelId, ?direction) = 
            Unmute (channelId, direction)
        static member Hold(channelId) = 
            Hold channelId
        static member Unhold(channelId) = 
            Unhold channelId
        static member StartMOH(channelId, ?mohClass) =
            StartMOH (channelId, mohClass)
        static member StopMOH(channelId) =
            StopMOH channelId

        static member StartSilence(channelId) = 
            StartSilence channelId
        static member StopSilence(channelId) =
            StopSilence channelId

        static member Play(channelId, media, ?lang, ?offsetms, ?skipms, ?playbackId) =
            {
                Play.channelId = channelId
                media = media
                lang = lang
                offsetms = offsetms
                skipms = skipms
                playbackId = playbackId
            }
        static member PlayWithId(channelId, playbackId, media, ?lang, ?offsetms, ?skipms) =
            {
                Play.channelId = channelId
                media = media
                lang = lang
                offsetms = offsetms
                skipms = skipms
                playbackId = playbackId
            }
        static member Record(channelId, name, format, ?maxDurationSeconds, ?maxSilenceSeconds, ?ifExists, ?beep, ?terminateOn) =
            {
                Record.channelId = channelId
                name = name
                format = format
                maxDurationSeconds = maxDurationSeconds
                maxSilenceSeconds = maxSilenceSeconds
                ifExists = ifExists
                beep = beep
                terminateOn = terminateOn
            }
        static member GetChannelVar(channelId, variable) =
            GetChannelVar (channelId, variable)
        static member SetChannelVar(channelId, variable, ?value) = 
            SetChannelVar (channelId, variable, value)
        static member SnoopChannel(channelId, app, ?spy, ?whisper, ?appArgs, ?snoopId) = 
            {
                SnoopChannel.channelId = channelId
                app = app
                spy = spy
                whisper = whisper
                appArgs = appArgs
                snoopId = snoopId
            }
        static member SnoopChannelWithId(channelId, snoopId, app, ?spy, ?whisper, ?appArgs) = 
            {
                SnoopChannel.channelId = channelId
                app = app
                spy = spy
                whisper = whisper
                appArgs = appArgs
                snoopId = snoopId
            }

[<assembly:AutoOpen("IVR.Asterisk.Commands")>]
do ()

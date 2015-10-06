namespace IVR.Asterisk

open System
open AsterNET.ARI
open AsterNET.ARI.Models
open IVR

module ClientExtensions = 

    open IVR

    let inline private optstr (o: string option) = 
        match o with
        | Some s -> s
        | None -> null

    let inline private optint (i: int option) = 
        match i with
        | Some i -> Nullable(i)
        | None -> Nullable()

    type List = List
        with
        interface IReturns<Channel list>

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
        }
        with 
        interface IReturns<Channel>

    type Get = Get of channelId: string
        with 
        interface IReturns<Channel>

    type ContinueInDialplan = {
        channelId: string
        context: string option
        extension: string option
        priority: int option
        label: string option
        }

    type SendDTMF = {
        channelId: string
        dtmf: string
        before: int option
        between: int option
        duration: int option
        after: int option
        }

    type Play = {
        channelId: string
        media: string
        lang: string option
        offsetms: int option
        skipms: int option
        playbackId: string option
        }
        with 
        interface IReturns<Playback> 

    type Record = {
        channelId: string
        name: string
        format: string
        maxDurationSeconds: int option
        maxSilenceSeconds: int option
        ifExists: string option
        beep: bool option
        terminateOn: string option
        }
        with 
        interface IReturns<LiveRecording>

    type GetChannelVar = GetChannelVar of channelId: string * variable: string
        with
        interface IReturns<Variable>
   
    type SnoopChannel = {
        channelId: string
        app: string
        spy: string option
        whisper: string option
        appArgs: string option
        snoopId: string option 
        }
        with
        interface IReturns<Channel>

    /// Channels command that do not have a return type are combined in one union 
    /// to simplify host processing. Don't use these constructors, use Channel.[name]
    /// instead.
    type ChannelsCommand = 
        | Hangup of channelId: string * reason: string option
        | ContinueInDialplan of ContinueInDialplan
        | Redirect of channelId: string * endpoint: string
        | Answer of channelId: string
        | Ring of channelId: string
        | RingStop of channelId: string
        | SendDTMF of SendDTMF
        | Mute of channelId: string * direction: string option
        | Unmute of channelId: string * direction: string option
        | Hold of channelId: string
        | Unhold of channelId: string
        | StartMOH of channelId: string * mohClass: string option
        | StopMOH of channelId: string
        | StartSilence of channelId: string
        | StopSilence of channelId: string
        | SetChannelVar of channelId: string * variable: string * value: string option
    
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
        static member Hangup(channelId, ?reason) =
            Hangup (channelId, reason)
        static member ContinueInDialplan(channelId, ?context, ?extension, ?priority, ?label) =
            ContinueInDialplan {
                channelId = channelId
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
            SendDTMF {
                channelId = channelId
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

[<assembly:AutoOpen("IVR.Asterisk.ClientExtensions")>]
do ()

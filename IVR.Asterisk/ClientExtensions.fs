namespace IVR.Asterisk

open System
open AsterNET.ARI
open AsterNET.ARI.Models
open IVR

module ClientExtensions = 

    let inline private optstr (o: string option) = 
        match o with
        | Some s -> s
        | None -> null

    let inline private optint (i: int option) = 
        match i with
        | Some i -> Nullable(i)
        | None -> Nullable()


    type IResultType<'result> = 
        interface end

    type List = List
        with
        interface IResultType<Channel list>

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
        interface IResultType<Channel>

    type Get = Get of channelId: string
        with 
        interface IResultType<Channel>

    type Hangup = {
        channelId: string
        reason: string option
        }
        with
        interface IResultType<unit>

    type ContinueInDialplan = {
        channelId: string
        context: string option
        extension: string option
        priority: int option
        label: string option
        }
        with
        interface IResultType<unit>

    type Redirect = {
        channelId: string
        endpoint: string
        }
        with
        interface IResultType<unit>

    type Answer = Answer of channelId: string
        with 
        interface IResultType<unit>

    type Ring = Ring of channelId: string
        with
        interface IResultType<unit>

    type RingStop = RingStop of channelId: string
        with
        interface IResultType<unit>

    type SendDTMF = {
        channelId: string
        dtmf: string
        before: int option
        between: int option
        duration: int option
        after: int option
        }
        with 
        interface IResultType<unit>

    type Mute = {
        channelId: string
        direction: string option
        }
        with
        interface IResultType<unit>

    type Unmute = {
        channelId: string
        direction: string option
        }
        with
        interface IResultType<unit>

    [<AbstractClass; Sealed>]   
    type Channels() =
        static member List() = List

    [<AbstractClass; Sealed>]
    type Channel() = 

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
            {
                channelId = channelId
                reason = reason
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
            {
                Redirect.channelId = channelId
                endpoint = endpoint
            }
        static member Answer(channelId) =
            Answer channelId
        static member Ring(channelId) = 
            Ring channelId
        static member RingStop(channelId) = 
            RingStop channelId
        static member SendDTMF(channelId, dtmf, ?before, ?between, ?duration, ?after) =
            {
                channelId = channelId
                dtmf = dtmf
                before = before
                between = between
                duration = duration
                after = after
            }
        static member Mute(channelId, ?direction) =
            {
                Mute.channelId = channelId
                direction = direction
            }
        static member Unmute(channelId, ?direction) = 
            {
                Unmute.channelId = channelId
                direction = direction
            }
#if false
        static member Hold(channelId) = 
            Hold channelId
        static member Unhold(channelId) = 
            Unhold chnanelId
        static member StartMOH(channelId, ?mohClass) =
            StartMOH (channelId, mohClass)
        static member StopMOH(channelId) =
            StopMOH channelId

        static member StartSilence(channelId) = 
            StartSilence channelId
        static member StopSilence(channelId) =
            StopSilence channelId
#endif

    type AriClient with
        
        // member this.list
        // member this.originate
        // member this.get
        // member this.originateWithId

        member this.hangup(channelId, ?reason) = this.Channels.Hangup(channelId, optstr reason) 

        // member this.continueInDialplan
        // member this.redirect

        member this.answer(channelId) = this.Channels.Answer(channelId)

        member this.ring(channelId) = this.Channels.Ring(channelId)

        member this.ringStop(channelId) = this.Channels.RingStop(channelId)

        // member this.sendDTMF
        // member this.mute
        // member this.umute
        // member this.unhold
        // member this.startMOH
        // member this.stopMOH
        // member this.startSilence
        // member this.stopSilence

        member this.beginPlay(channelId, media, ?lang, ?offsetms, ?skipms, ?playbackId) =
            this.Channels.Play(channelId, media, optstr lang, optint offsetms, optint skipms, optstr playbackId)

        member this.play(channelId, media, ?lang : string, ?offsetms, ?skipms, ?playbackId) =
            let playback = this.beginPlay(channelId, media, ?lang = lang, ?offsetms = offsetms, ?skipms = skipms, ?playbackId = playbackId)
            IVR.waitFor' (fun (e: PlaybackFinishedEvent) -> e.Playback.Id = playback.Id)

        // member this.playWithId

        member this.record(channelId, name, format, ?maxDurationSeconds) =
            this.Channels.Record(channelId, name, format, optint maxDurationSeconds)

        // member this.getChannelVar
        // member this.setChannelVar
        // member this.snoopChannel
        // member this.snoopChannelWithId

[<assembly:AutoOpen("IVR.Asterisk.ClientExtensions")>]
do ()

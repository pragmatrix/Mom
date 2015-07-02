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

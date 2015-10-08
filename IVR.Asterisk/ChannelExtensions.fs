namespace IVR.Asterisk

open AsterNET.ARI.Models
open IVR
open Commands

module ChannelExtensions =
    
    // https://wiki.asterisk.org/wiki/display/AST/Asterisk+13+Channels+REST+API#Asterisk13ChannelsRESTAPI-list
            
    type Channel with

        member inline private this.waitFor< ^e when ^e : (member Channel : Channel)>() =
            let f e =
                let channel = (^e : (member Channel : Channel) (e))
                if channel.Id = this.Id then Some e else None

            IVR.waitFor f

        member this.waitForStasisEnd() = this.waitFor<StasisEndEvent>()

        member this.waitForCallerId() = this.waitFor<ChannelCallerIdEvent>()
        member this.waitForConnectedLine() = this.waitFor<ChannelConnectedLineEvent>()
        member this.waitForCreated() = this.waitFor<ChannelCreatedEvent>()
        member this.waitForDestroyed() = this.waitFor<ChannelDestroyedEvent>()
        member this.waitForDialplan() = this.waitFor<ChannelDialplanEvent>()
        member this.waitForDTMFReceived() = this.waitFor<ChannelDtmfReceivedEvent>()
        member this.waitForEnteredBridge() = this.waitFor<ChannelEnteredBridgeEvent>()
        member this.waitForHangupRequest() = this.waitFor<ChannelHangupRequestEvent>()
        member this.waitForLeftBridge() = this.waitFor<ChannelLeftBridgeEvent>()
        member this.waitForStateChange() = this.waitFor<ChannelStateChangeEvent>()
        member this.waitForTalkingFinished() = this.waitFor<ChannelTalkingFinishedEvent>()
        member this.waitForTalkingStarted() = this.waitFor<ChannelTalkingStartedEvent>()
        member this.waitForUserEvent() = this.waitFor<ChannelUsereventEvent>()
        member this.waitForVarsetEvent() = this.waitFor<ChannelVarsetEvent>()

        member this.waitForKey (key: char) = 
            ivr {
                let! e = this.waitForDTMFReceived()
                if e.Digit.[0] <> key then
                    return! this.waitForKey key
                else 
                    return ()
            }

        member this.hangup(?reason) = 
            Channels.Hangup(this.Id, ?reason = reason)
        member this.continueInDialplan(?context, ?extension, ?priority, ?label) =
            Channels.ContinueInDialplan(this.Id, ?context = context, ?extension = extension, ?priority = priority, ?label = label)
        member this.redirect(endpoint) = 
            Channels.Redirect(this.Id, endpoint)
        member this.answer() = 
            Channels.Answer(this.Id)
        member this.ring() = 
            Channels.Ring(this.Id)
        member this.ringStop() = 
            Channels.RingStop(this.Id)
        member this.sendDTMF(dtmf, ?before, ?between, ?duration, ?after) = 
            Channels.SendDTMF(this.Id, dtmf, ?before = before, ?between = between, ?duration = duration, ?after = after)
        member this.mute(?direction) =
            Channels.Mute(this.Id, ?direction = direction)
        member this.unmute(?direction) = 
            Channels.Unmute(this.Id, ?direction = direction)
        member this.hold() = 
            Channels.Hold(this.Id)
        member this.unhold() = 
            Channels.Unhold(this.Id)
        member this.startMOH(?mohClass) =
            Channels.StartMOH(this.Id, ?mohClass = mohClass)
        member this.stopMOH() = 
            Channels.StopMOH(this.Id)
        member this.startSilence() = 
            Channels.StartSilence(this.Id)
        member this.stopSilence() = 
            Channels.StopSilence(this.Id)
        member this.play(media, ?lang, ?offset, ?skip, ?playbackId) = 
            Channels.Play(this.Id, media, ?lang = lang, ?offset = offset, ?skip = skip, ?playbackId = playbackId)
        member this.record(name, format, ?maxDuration, ?maxSilence, ?ifExists, ?beep, ?terminateOn) =
            Channels.Record(this.Id, name, format, ?maxDuration = maxDuration, ?maxSilence = maxSilence, ?ifExists = ifExists, ?beep = beep, ?terminateOn = terminateOn)
        member this.getVar(variable) = 
            Channels.GetChannelVar(this.Id, variable)
        member this.setVar(variable, ?value) = 
            Channels.SetChannelVar(this.Id, variable, ?value = value)
        member this.snoop(app, ?spy, ?whisper, ?appArgs, ?snoopId) = 
            Channels.SnoopChannel(this.Id, app, ?spy = spy, ?whisper = whisper, ?appArgs = appArgs, ?snoopId = snoopId)

        /// An IVR that plays a media, and waits for the PlaybackFinishedEvent
        member this.play'(media, ?lang, ?offset, ?skip) : unit ivr= 
            ivr {
                let! playback = 
                    this.play(media, ?lang = lang, ?offset = offset, ?skip = skip)
                    |> IVR.send
                do! IVR.waitForPlaybackFinished(playback.Id)
            }

[<assembly:AutoOpen("IVR.Asterisk.ChannelExtensions")>]
do ()

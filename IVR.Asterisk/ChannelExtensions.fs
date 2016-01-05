namespace IVR.Asterisk

open AsterNET.ARI.Models
open IVR

open Channels

module ChannelExtensions =
    
    type ChannelState =
        | Down
        | Reserved
        | OffHook
        | Dialing
        | Ring
        | Ringing
        | Up
        | Busy
        | DialingOffhook
        | PreRing
        | Unknown
        with
        // https://wiki.asterisk.org/wiki/display/AST/Asterisk+11+ManagerEvent_Newstate
        static member parse str =
            match str with
            | "Down" -> Down
            | "Rsrvd" -> Reserved
            | "OffHook" -> OffHook
            | "Dialing" -> Dialing
            | "Ring" -> Ring
            | "Ringing" -> Ringing
            | "Up" -> Up
            | "Busy" -> Busy
            | "Dialing Offhook" -> DialingOffhook
            | "Pre-ring" -> PreRing
            | "Unknown" -> Unknown
            | _ -> Unknown

    // https://wiki.asterisk.org/wiki/display/AST/Asterisk+13+Channels+REST+API#Asterisk13ChannelsRESTAPI-list
            
    type Channel with

        member inline private this.waitFor< ^e,'r when ^e : (member Channel : Channel)>(guard: ^e -> 'r option) =
            let f e =
                let channel = (^e : (member Channel : Channel) (e))
                if channel.Id = this.Id then guard e else None

            IVR.waitFor f

        member inline private this.waitFor< ^e when ^e : (member Channel : Channel)>() : ^e ivr =
            this.waitFor(Some)

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
        // 13.5
        member this.waitForChannelHold() = this.waitFor<ChannelHoldEvent>()
        member this.waitForChannelUnholdEvent() = this.waitFor<ChannelUnholdEvent>()

        /// Waits for a ChannelStateChangeEvent that changes to the given state.
        member this.waitForChannelState(state: ChannelState) = 
            this.waitFor<ChannelStateChangeEvent, _>(fun changeEvent -> if changeEvent.Channel.state = state then Some changeEvent else None)

        member this.waitForKey (key: char) = 
            ivr {
                let! e = this.waitForDTMFReceived()
                if e.Digit.[0] <> key then
                    return! this.waitForKey key
                else 
                    return ()
            }

        member this.state = this.State |> ChannelState.parse

        member this.hangup(?reason) = 
            Channels.Channels.Hangup(this.Id, ?reason = reason)
        member this.continueInDialplan(?context, ?extension, ?priority, ?label) =
            Channels.Channels.ContinueInDialplan(this.Id, ?context = context, ?extension = extension, ?priority = priority, ?label = label)
        member this.redirect(endpoint) = 
            Channels.Channels.Redirect(this.Id, endpoint)
        member this.answer() = 
            Channels.Channels.Answer(this.Id)
        member this.ring() = 
            Channels.Channels.Ring(this.Id)
        member this.ringStop() = 
            Channels.Channels.RingStop(this.Id)
        member this.sendDTMF(dtmf, ?before, ?between, ?duration, ?after) = 
            Channels.Channels.SendDTMF(this.Id, dtmf, ?before = before, ?between = between, ?duration = duration, ?after = after)
        member this.mute(?direction) =
            Channels.Channels.Mute(this.Id, ?direction = direction)
        member this.unmute(?direction) = 
            Channels.Channels.Unmute(this.Id, ?direction = direction)
        member this.hold() = 
            Channels.Channels.Hold(this.Id)
        member this.unhold() = 
            Channels.Channels.Unhold(this.Id)
        member this.startMOH(?mohClass) =
            Channels.Channels.StartMOH(this.Id, ?mohClass = mohClass)
        member this.stopMOH() = 
            Channels.Channels.StopMOH(this.Id)
        member this.startSilence() = 
            Channels.Channels.StartSilence(this.Id)
        member this.stopSilence() = 
            Channels.Channels.StopSilence(this.Id)
        member this.play(media, ?lang, ?offset, ?skip, ?playbackId) = 
            Channels.Channels.Play(this.Id, media, ?lang = lang, ?offset = offset, ?skip = skip, ?playbackId = playbackId)
        member this.record(name, format, ?maxDuration, ?maxSilence, ?ifExists, ?beep, ?terminateOn) =
            Channels.Channels.Record(this.Id, name, format, ?maxDuration = maxDuration, ?maxSilence = maxSilence, ?ifExists = ifExists, ?beep = beep, ?terminateOn = terminateOn)
        member this.getVar(variable) = 
            Channels.Channels.GetChannelVar(this.Id, variable)
        member this.setVar(variable, ?value) = 
            Channels.Channels.SetChannelVar(this.Id, variable, ?value = value)
        member this.snoop(app, ?spy, ?whisper, ?appArgs, ?snoopId) = 
            Channels.Channels.SnoopChannel(this.Id, app, ?spy = spy, ?whisper = whisper, ?appArgs = appArgs, ?snoopId = snoopId)

        /// An IVR that plays a media, and waits for the PlaybackFinishedEvent
        member this.play'(media, ?lang, ?offset, ?skip) : unit ivr= 
            ivr {
                let! playback = this.play(media, ?lang = lang, ?offset = offset, ?skip = skip)
                do! IVR.waitForPlaybackFinished(playback.Id)
            }

[<assembly:AutoOpen("IVR.Asterisk.ChannelExtensions")>]
do ()

namespace IVR.Asterisk

module Extensions =
    
    open System
    open AsterNET.ARI.Models
    open IVR
    open Applications
    open Channels

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


    /// Id of the Asterisk Channel
    type ChannelId = ChannelId of string
        with
        member this.value = let (ChannelId v) = this in v
        member this.uri = "channel:" + this.value |> Uri

    /// https://wiki.asterisk.org/wiki/display/AST/Hangup+Cause+Mappings
    type AsteriskHangupCause = 
        | NotDefined = 0
        | Unallocated = 1
        | NoRouteTransitNet = 2
        | NoRouteDestination = 3
        | MisdialledTrunkPrefix = 5
        | ChannelUnacceptable = 6
        | CallAwardedDelivered = 7
        | PreEmpted = 8
        | NumberPortedNotHere = 14
        | NormalClearing = 16
        | UserBusy = 17
        | NoUserResponse = 18
        | NoAnswer = 19
        | SubscriberAbsent = 20
        | CallRejected = 21
        | NumberChanged = 22
        | RedirectedToNewDestination = 23
        | AnsweredElsewhere = 26
        | DestinationOutOfOrder = 27
        | InvalidNumberFormat = 28
        | FacilityRejected = 29
        | ResponseToStatusEnquiry = 30
        | NormalUnspecified = 31
        | NormalCircuitCongestion = 34
        | NetworkOutOfOrder = 38 
        | NormalTemporaryFailure = 41
        | SwitchCongestion = 42
        | AccessInfoDiscarded = 43
        | RequestedChannelUnavailable = 44
        | FacilityNotSubscribed = 50
        | OutgoingCallBarred = 52
        | IncomingCallBarred = 54
        | BearerCapabilityNotAuthorized = 57
        | BearerCapabilityNotAvailable = 58
        | BearerCapabilityNotImplemented = 65
        | ChannelTypeNotImplemented = 66
        | FacilityNotImplemented = 69
        | InvalidCallReference = 81
        | IncompatibleDestination = 88 
        | InvalidMessageUnspecified = 95
        | MandatoryInformationElementMissing = 96
        | MessageTypeNonExistent = 97
        | WrongMessage = 98
        | InformationElementNonExistent = 99
        | InvalidInformationElementContents = 100
        | WrongCallState = 101
        | RecoveryOnTimerExpire = 102
        | UnspecifiedProtocolError = 111
        | UnspecifiedInternetworking = 127

    type HangupCause = HangupCause of int
        with
        member this.value = let (HangupCause v) = this in v

    // https://wiki.asterisk.org/wiki/display/AST/Asterisk+13+Channels+REST+API#Asterisk13ChannelsRESTAPI-list

    module Application = 
        let waitForReplaced (applicationName: string) = 
            fun (e: ApplicationReplacedEvent) ->
                e.Application = applicationName 
            |> IVR.waitFor'
            
    type Channel with

        member this.id = this.Id |> ChannelId

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
            Channels.Channels.hangup(this.Id, ?reason = reason)
        member this.continueInDialplan(?context, ?extension, ?priority, ?label) =
            Channels.Channels.continueInDialplan(this.Id, ?context = context, ?extension = extension, ?priority = priority, ?label = label)
        member this.redirect(endpoint) = 
            Channels.Channels.redirect(this.Id, endpoint)
        member this.answer() = 
            Channels.Channels.answer(this.Id)
        member this.ring() = 
            Channels.Channels.ring(this.Id)
        member this.ringStop() = 
            Channels.Channels.ringStop(this.Id)
        member this.sendDTMF(dtmf, ?before, ?between, ?duration, ?after) = 
            Channels.Channels.sendDTMF(this.Id, dtmf, ?before = before, ?between = between, ?duration = duration, ?after = after)
        member this.mute(?direction) =
            Channels.Channels.mute(this.Id, ?direction = direction)
        member this.unmute(?direction) = 
            Channels.Channels.unmute(this.Id, ?direction = direction)
        member this.hold() = 
            Channels.Channels.hold(this.Id)
        member this.unhold() = 
            Channels.Channels.unhold(this.Id)
        member this.startMOH(?mohClass) =
            Channels.Channels.startMOH(this.Id, ?mohClass = mohClass)
        member this.stopMOH() = 
            Channels.Channels.stopMOH(this.Id)
        member this.startSilence() = 
            Channels.Channels.startSilence(this.Id)
        member this.stopSilence() = 
            Channels.Channels.stopSilence(this.Id)
        member this.play(media, ?lang, ?offset, ?skip, ?playbackId) = 
            Channels.Channels.play(this.Id, media, ?lang = lang, ?offset = offset, ?skip = skip, ?playbackId = playbackId)
        member this.record(name, format, ?maxDuration, ?maxSilence, ?ifExists, ?beep, ?terminateOn) =
            Channels.Channels.record(this.Id, name, format, ?maxDuration = maxDuration, ?maxSilence = maxSilence, ?ifExists = ifExists, ?beep = beep, ?terminateOn = terminateOn)
        member this.getVar(variable) = 
            Channels.Channels.getChannelVar(this.Id, variable)
        member this.setVar(variable, ?value) = 
            Channels.Channels.getChannelVar(this.Id, variable, ?value = value)
        member this.snoop(app, ?spy, ?whisper, ?appArgs, ?snoopId) = 
            Channels.Channels.snoopChannel(this.Id, app, ?spy = spy, ?whisper = whisper, ?appArgs = appArgs, ?snoopId = snoopId)

        /// An IVR that plays a media, and waits for the PlaybackFinishedEvent
        member this.play'(media, ?lang, ?offset, ?skip) : unit ivr= 
            ivr {
                let! playback = this.play(media, ?lang = lang, ?offset = offset, ?skip = skip)
                do! IVR.waitForPlaybackFinished(playback.Id)
            }

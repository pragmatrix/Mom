namespace IVR.Asterisk

open AsterNET.ARI.Models
open IVR

module ChannelExtensions =
        
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

        member this.waitFor (key: char) = 
            ivr {
                let! (e:ChannelDtmfReceivedEvent) = this.waitForDTMFReceived()
                if e.Digit.[0] <> key then
                    return! this.waitFor key
                else 
                    return ()
            }
            

[<assembly:AutoOpen("IVR.Asterisk.ChannelExtensions")>]
do ()

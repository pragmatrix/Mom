namespace IVR.Asterisk

open System
open System.Threading

open AsterNET.ARI
open AsterNET.ARI.Actions
open AsterNET.ARI.Models
open AsterNET.ARI.Middleware

open IVR.Threading

module Client = 

#if false
    type ARIEvent =
        | DeviceStateChanged of DeviceStateChangedEvent
        | PlaybackStarted of PlaybackStartedEvent
        | PlaybackFinished of PlaybackFinishedEvent
        | RecordingStarted of RecordingStartedEvent
        | RecordingFinished of RecordingFinishedEvent
        | RecordingFailed of RecordingFailedEvent
        | ApplicationReplaced of ApplicationReplacedEvent
        | BridgeCreated of BridgeCreatedEvent
        | BridgeDestroyed of BridgeDestroyedEvent
        | BridgeMerged of BridgeMergedEvent
        | BridgeBlindTransfer of BridgeBlindTransferEvent
        | BridgeAttendedTransfer of BridgeAttendedTransferEvent
        | ChannelCreated of ChannelCreatedEvent
        | ChannelDestroyed of ChannelDestroyedEvent
        | ChannelEnteredBridge of ChannelEnteredBridgeEvent
        | ChannelLeftBridge of ChannelLeftBridgeEvent
        | ChannelStateChange of ChannelStateChangeEvent
        | ChannelDtmfReceived of ChannelDtmfReceivedEvent
        | ChannelDialplan of ChannelDialplanEvent
        | ChannelCallerId of ChannelCallerIdEvent
        | ChannelUserevent of ChannelUsereventEvent
        | ChannelHangupRequest of ChannelHangupRequestEvent
        | ChannelVarset of ChannelVarsetEvent
        | ChannelTalkingStarted of ChannelTalkingStartedEvent
        | ChannelTalkingFinished of ChannelTalkingFinishedEvent
        | EndpointStateChange of EndpointStateChangeEvent
        | Dial of DialEvent
        | StasisEnd of StasisEndEvent
        | StasisStart of StasisStartEvent
        | TextMessageReceived of TextMessageReceivedEvent
        | ChannelConnectedLine of ChannelConnectedLineEvent
        | Unknown of Event
        with
            static member import (e : Event) =
                match e.Type with
                | "DeviceStateChanged" -> e :?> DeviceStateChangedEvent |> DeviceStateChanged
                | "PlaybackStarted" -> e :?> PlaybackStartedEvent |> PlaybackStarted
                | "PlaybackFinished" -> e :?> PlaybackFinishedEvent |> PlaybackFinished
                | "RecordingStarted" -> e :?> RecordingStartedEvent |> RecordingStarted
                | "RecordingFinished" -> e :?> RecordingFinishedEvent |> RecordingFinished
                | "RecordingFailed" -> e :?> RecordingFailedEvent |> RecordingFailed
                | "ApplicationReplaced" -> e :?> ApplicationReplacedEvent |> ApplicationReplaced
                | "BridgeCreated" -> e :?> BridgeCreatedEvent |> BridgeCreated
                | "BridgeDestroyed" -> e :?> BridgeDestroyedEvent |> BridgeDestroyed
                | "BridgeMerged" -> e :?> BridgeMergedEvent |> BridgeMerged
                | "BridgeBlindTransfer" -> e :?> BridgeBlindTransferEvent |> BridgeBlindTransfer
                | "BridgeAttendedTransfer" -> e :?> BridgeAttendedTransferEvent |> BridgeAttendedTransfer
                | "ChannelCreated" -> e :?> ChannelCreatedEvent |> ChannelCreated
                | "ChannelDestroyed" -> e :?> ChannelDestroyedEvent |> ChannelDestroyed
                | "ChannelEnteredBridge" -> e :?> ChannelEnteredBridgeEvent |> ChannelEnteredBridge
                | "ChannelLeftBridge" -> e :?> ChannelLeftBridgeEvent |> ChannelLeftBridge
                | "ChannelStateChange" -> e :?> ChannelStateChangeEvent |> ChannelStateChange
                | "ChannelDtmfReceived" -> e :?> ChannelDtmfReceivedEvent |> ChannelDtmfReceived
                | "ChannelDialplan" -> e :?> ChannelDialplanEvent |> ChannelDialplan
                | "ChannelCallerId" -> e :?> ChannelCallerIdEvent |> ChannelCallerId
                | "ChannelUserevent" -> e :?> ChannelUsereventEvent |> ChannelUserevent
                | "ChannelHangupRequest" -> e :?> ChannelHangupRequestEvent |> ChannelHangupRequest
                | "ChannelVarset" -> e :?> ChannelVarsetEvent |> ChannelVarset
                | "ChannelTalkingStarted" -> e :?> ChannelTalkingStartedEvent |> ChannelTalkingStarted
                | "ChannelTalkingFinished" -> e :?> ChannelTalkingFinishedEvent |> ChannelTalkingFinished
                | "EndpointStateChange" -> e :?> EndpointStateChangeEvent |> EndpointStateChange
                | "Dial" -> e :?> DialEvent |> Dial
                | "StasisEnd" -> e :?> StasisEndEvent |> StasisEnd
                | "StasisStart" -> e :?> StasisStartEvent |> StasisStart
                | "TextMessageReceived" -> e :?> TextMessageReceivedEvent |> TextMessageReceived
                | "ChannelConnectedLine" -> e :?> ChannelConnectedLineEvent |> ChannelConnectedLine
                | _ -> e |> Unknown
#endif

    type ARIClientEvent =
        | ARIEvent of obj
        | Connected
        | Disconnected

    type ARIConnection(disconnect: unit -> unit) =
        interface IDisposable with
            member this.Dispose() = 
                disconnect()

    type ARIConnectionWithQueue(queue: SynchronizedQueue<ARIClientEvent>, disconnect: unit -> unit) =
        interface IDisposable with
            member this.Dispose() =
                disconnect()

        member this.nextEvent() = 
            queue.dequeue()

    type AriClient with

        member this.connect() = 

            let queue = SynchronizedQueue()
            let connection = this.connect(queue.enqueue)
            new ARIConnectionWithQueue(queue, (connection :> IDisposable).Dispose)

        member this.connect(f : ARIClientEvent -> unit) =
            
            let connectionStateChanged f (_:obj) = 
                match this.ConnectionState with
                | ConnectionState.Closed -> f Disconnected
                | ConnectionState.Open -> f Connected
                | _ -> ()

            let unhandledEvent _ event = 
                event |> ARIEvent |> f

            let eventHandler = UnhandledEventHandler unhandledEvent

            this.OnUnhandledEvent.AddHandler eventHandler

            let cleanup() =
                this.OnUnhandledEvent.RemoveHandler eventHandler

            // we never want to handle auto reconnect, the application should completely fail, when the connection
            // gets disconnected, we consider this an application failure, and not a temporary glitch of the connection
            // An application failure usually has a much greater impact and will be reported, and that's what we actually
            // want here!

            // later on, we will poll for the Connected flag to find out when the connection closes.

            let connectionQueue = SynchronizedQueue()

            let connectionHandler = AriClient.ConnectionStateChangedHandler (connectionStateChanged connectionQueue.enqueue)
            this.OnConnectionStateChanged.AddHandler connectionHandler
                
            this.Connect(autoReconnect = false)
            assert(this.ConnectionState = Middleware.ConnectionState.Connecting)

            try
                match connectionQueue.dequeue() with
                | Connected -> 
                    let connectedHandler = AriClient.ConnectionStateChangedHandler(connectionStateChanged f)
                    this.OnConnectionStateChanged.AddHandler connectedHandler

                    let cleanup() = 
                        this.OnConnectionStateChanged.RemoveHandler connectedHandler
                        this.Disconnect()
                        cleanup()
                    new ARIConnection(cleanup)

                | _ ->
                    failwithf "failed to connect to ARI, connection state is %s" (this.ConnectionState.ToString())

            finally
                this.OnConnectionStateChanged.RemoveHandler connectionHandler
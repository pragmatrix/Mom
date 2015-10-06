namespace IVR.Tests

open System

open IVR

open IVR.Asterisk
open IVR.Asterisk.Client

open AsterNET.ARI
open AsterNET.ARI.Models

open NUnit.Framework

(*
    Live Tests run with a full active ARI client in real time.

    To configure asterisk to run the following tests, enable http and ARI:

    https://wiki.asterisk.org/wiki/display/AST/Asterisk+Configuration+for+ARI
    
    and add the following lines to extensions.conf:

    exten => [the extension, or s for catch-all],1,NoOp()
    same => n,Stasis(IVR.Test.Application)
    same => n,Hangup()

    And point host to the IP of the asterisk server (or change the host entry 'asterisk').
*)

module Configuration = 
    let host = "asterisk"
    let port = 8088
    let user = "asterisk"
    let password = "password"

    let endpoint = StasisEndpoint(host, port, user, password)
    let applicationName = "IVR.Test.Application"

[<TestFixture;Category("DependsOnHost")>]
type LiveTests() =

    [<Test>]
    member this.failedConnect() =

        let failingEndpoint = StasisEndpoint("____unresolvable_host_____", 8088, "", "")

        use client = new AriClient(failingEndpoint, Configuration.applicationName)
        try
            use connection = client.connect()
            Assert.Fail()
        with e ->
            ()

    [<Test>]
    member this.succeedingConnect() = 
 
        use client = new AriClient(Configuration.endpoint, Configuration.applicationName)
        use x = client.connect()

        Assert.True(client.Connected)
        
    [<Test>]
    member this.runAndDumpEvents() = 

        use client = new AriClient(Configuration.endpoint, Configuration.applicationName)
        use connection = client.connect()

        while true do
            let msg = connection.nextEvent()
            System.Diagnostics.Debug.WriteLine(sprintf "%A" msg)


    [<Test>]
    member this.subscribeToChannelOverASecondClient() =

        // test that if we connect via a second client, we can subscribe to the
        // channel, this is required to process channels independently with
        // separate processes.

        use client = new AriClient(Configuration.endpoint, Configuration.applicationName)
        use connection = client.connect()

        while true do
            let msg = connection.nextEvent()
            System.Diagnostics.Debug.WriteLine(sprintf "primary msg: %A" msg)
            
            match msg with
            | ARIEvent (:? StasisStartEvent as e) ->
                let id = e.Channel.Id
                let channelApp = Configuration.applicationName + "." + id.ToString()
                use client2 = new AriClient(Configuration.endpoint, channelApp)
                use connection = client2.connect()
                client2.Applications.Subscribe(channelApp, "channel:" + string id) |> ignore
                let msg = connection.nextEvent()
                System.Diagnostics.Debug.WriteLine(sprintf "secondary msg: %A" msg)
            | _ -> ()

    [<Test>]
    member this.parallelIVRBlocks() = 

        // this is a simple call flow that shows how to combine IVR blocks in parallel.

        use client = new AriClient(Configuration.endpoint, Configuration.applicationName)
        let runtime = IVR.Runtime.newRuntime(client.host)

        let delay = IVR.delay

        use connection = client.connect(runtime)
        
        let channelIVR (channel: Channel) = 
            ivr {
                yield channel.ring()
                do! delay (2 .seconds)
                yield channel.answer()
                do! delay (1 .seconds)
                do! channel.play' "sound:tt-weasels" 
                yield channel.hangup()
            }

        let hangupOn1 (channel: Channel) = 
            ivr {
                do! channel.waitForKey '1'
                yield channel.hangup()
            }
                        
        let handleHangupRequestByHangingUp (channel : Channel) = 
            ivr {
                do! channel.waitForHangupRequest() |> IVR.ignore
                yield channel.hangup()
            }

        let rec distributor() = 
            ivr {
                let! startEvent = IVR.waitForStasisStart()
                let channel = startEvent.Channel
                let activeChannelIVR =
                    IVR.waitAny [
                        channel.waitForStasisEnd() |> IVR.ignore
                        handleHangupRequestByHangingUp channel
                        channelIVR channel
                        hangupOn1 channel
                    ]

                return! 
                    // IVR.waitAll [distributor(); activeChannelIVR]
                    IVR.waitAll [activeChannelIVR]
                    |> IVR.ignore
            }

        runtime.run (distributor())
        |> ignore


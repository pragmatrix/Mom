namespace IVR.Tests

open NUnit.Framework
open FsUnit

open IVR

open IVR.Asterisk
open IVR.Asterisk.Client
open IVR.Asterisk.Extensions

open AsterNET.ARI
open AsterNET.ARI.Models

open System
open System.IO

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
type ARITests() =

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

    member this.createTestIVR() = 
        let channelIVR (channel: Channel) = 
            ivr {
                do! channel.ring()
                do! IVR.delay (2 .seconds)
                do! channel.answer()
                do! IVR.delay (1 .seconds)
                do! channel.play' (Uri("sound:tt-weasels"))
                do! channel.hangup()
            }

        let hangupOn1 (channel: Channel) = 
            ivr {
                do! channel.waitForKey '1'
                do! channel.hangup()
            }
                        
        let handleHangupRequestByHangingUp (channel : Channel) = 
            ivr {
                do! channel.waitForHangupRequest() |> IVR.ignore
                do! channel.hangup()
            }

        let rec distributor() = 
            ivr {
                let! startEvent = IVR.waitForStasisStart()
                let channel = startEvent.Channel
                let activeChannelIVR =
                    IVR.any [
                        channel.waitForStasisEnd() |> IVR.ignore
                        handleHangupRequestByHangingUp channel
                        channelIVR channel
                        hangupOn1 channel
                    ]

                return! 
                    // IVR.waitAll [distributor(); activeChannelIVR]
                    IVR.all [activeChannelIVR]
                    |> IVR.ignore
            }

        distributor


    [<Test>]
    member this.simpleTestIVR() = 

        // this is a simple call flow that shows how to combine IVR blocks in parallel.

        use client = new AriClient(Configuration.endpoint, Configuration.applicationName)
        let runtime = IVR.Runtime.newRuntime client.service

        use connection = client.connect(runtime)
        
        let distributor = this.createTestIVR()
        runtime.run (distributor())
        |> ignore


    [<Test>]
    member this.simpleTestIVRTraceRoundtrip() = 
        
        let distributor = this.createTestIVR()
        let traceFN = "simpleTestIVR.trace"
        
        let sessionInfo = Tracing.sessionInfo "" 0L null
        let fileTracer = Tracers.fileTracer traceFN sessionInfo
        let tracingDistributor = Tracing.trace fileTracer (distributor())

        use client = new AriClient(Configuration.endpoint, Configuration.applicationName)
        let runtime = IVR.Runtime.newRuntime client.service

        use connection = client.connect(runtime)
        
        runtime.run tracingDistributor
        |> ignore

        printfn "Trace size: %d" (FileInfo(traceFN).Length)

        let trace = Tracers.readFileTrace traceFN

        let report = 
            trace
            |> Tracing.replay distributor

        report.incidents |> Seq.iter (printfn "%A")
        report.isEmpty |> should equal true



        


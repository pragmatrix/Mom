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
*)

module Configuration = 
    let host = "192.168.1.21"
    let port = 8088
    let user = "asterisk"
    let password = "password"

    let endpoint = StasisEndpoint(host, port, user, password)
    let applicationName = "IVR.Test.Application"

[<AutoOpen>]
module LiveTest_ = 
    let ivr<'r> = IVR.ivr<'r>

[<TestFixture>]
type LiveTests() =


    [<Test>]
    member this.failedConnect() =

        let failingEndpoint = StasisEndpoint("____unresolvable_host_____", 8088, "", "")

        let client = AriClient(failingEndpoint, Configuration.applicationName)
        try
            use connection = client.connect()
            Assert.Fail()
        with e ->
            ()

    [<Test>]
    member this.succeedingConnect() = 
        // For this to succeed, the endpoint in Configuration must be reachable.
        //
        // to configure Asterisk ARI:
        // https://wiki.asterisk.org/wiki/display/AST/Asterisk+Configuration+for+ARI

        let client = AriClient(Configuration.endpoint, Configuration.applicationName)
        use x = client.connect()

        Assert.True(client.Connected)
        
    [<Test>]
    member this.runAndDumpEvents() = 

        let client = AriClient(Configuration.endpoint, Configuration.applicationName)
        use connection = client.connect()

        while true do
            let msg = connection.nextEvent()
            System.Diagnostics.Debug.WriteLine(sprintf "%A" msg)

    // test that if we connect via a second client, we can subscribe to the
    // channel, this is required for us to process channels independently with
    // separate processes

    [<Test>]
    member this.subscribeToChannelOverASecondClient() =

        let client = AriClient(Configuration.endpoint, Configuration.applicationName)
        use connection = client.connect()

        while true do
            let msg = connection.nextEvent()
            System.Diagnostics.Debug.WriteLine(sprintf "primary msg: %A" msg)
            
            match msg with
            | ARIEvent (:? StasisStartEvent as e) ->
                let id = e.Channel.Id
                let channelApp = Configuration.applicationName + "." + id.ToString()
                let client2 = AriClient(Configuration.endpoint, channelApp)
                use connection = client2.connect()
                client2.Applications.Subscribe(channelApp, "channel:" + string id) |> ignore
                let msg = connection.nextEvent()
                System.Diagnostics.Debug.WriteLine(sprintf "secondary msg: %A" msg)
            | _ -> ()

    [<Test>]
    member this.acceptACallAndHangupAfter5Seconds() = 

        let client = AriClient(Configuration.endpoint, Configuration.applicationName)
        let host = IVR.newHost()

        let deliverARIEventToHost event = 
            match event with
            | ARIEvent e -> host.dispatch e
            | Disconnected -> host.cancel()
            | _ -> failwith "unexpected ARI event: %A" event

        use connection = client.connect(deliverARIEventToHost)

        let answerAndHangup (channel: Channel) = ivr {
            let id = channel.Id
            client.Channels.Answer(id)
            do! host.delay (TimeSpan.FromSeconds(5.))
            client.Channels.Hangup(id)
        }
            
        let waitForStasisEnd (channel : Channel) = 
            IVR.waitFor' (fun (e: StasisEndEvent) -> e.Channel.Id = channel.Id) 

        let waitForHangupAndHangup (channel : Channel) = 
            ivr {
                do! IVR.waitFor' (fun (e: ChannelHangupRequestEvent) -> e.Channel.Id = channel.Id)
                client.Channels.Hangup(channel.Id)
            }

        let rec distributor() = ivr {
            let! stasisStart = IVR.waitFor (fun (e: StasisStartEvent) -> e)
            let channel = stasisStart.Channel
            let channelIVR =
                [waitForStasisEnd channel; waitForHangupAndHangup channel; answerAndHangup channel]
                |> IVR.lpar'
            return! 
                [distributor(); channelIVR]
                |> IVR.lpar 
                |> IVR.ignore
        }

        IVR.run (distributor()) host
        |> ignore


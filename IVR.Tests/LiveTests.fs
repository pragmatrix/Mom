namespace IVR.Tests

open IVR.Asterisk
open IVR.Asterisk.Client

open AsterNET.ARI

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
            let msg = connection.poll()
            System.Diagnostics.Debug.WriteLine(sprintf "%A" msg)

    // test that if we connect via a second client, we can subscribe to the
    // channel, this is required for us to process channels independently with
    // separate processes

    [<Test>]
    member this.subscribeToChannelOverASecondClient() =

        let client = AriClient(Configuration.endpoint, Configuration.applicationName)
        use connection = client.connect()

        while true do
            let msg = connection.poll()
            System.Diagnostics.Debug.WriteLine(sprintf "primary msg: %A" msg)
            
            match msg with
            | ARIEvent (StasisStart e) ->
                let id = e.Channel.Id
                let channelApp = Configuration.applicationName + "." + id.ToString()
                let client2 = AriClient(Configuration.endpoint, channelApp)
                use connection = client2.connect()
                client2.Applications.Subscribe(channelApp, "channel:" + string id) |> ignore
                let msg = connection.poll()
                System.Diagnostics.Debug.WriteLine(sprintf "secondary msg: %A" msg)
            | _ -> ()

module IVR.Tests.RuntimeTests

open System
open System.Threading
open FsUnit
open Xunit
open IVR

exception AsyncException of string

[<Fact>]
let ``host properly cancels its ivr if the runtime gets disposed asynchronously``() =
    let ct = new IVRTests.CancellationTracker()

    let ivr = ivr {
        use ct = ct
        do! IVR.waitFor' (fun _ -> true)
    }

    let host _ _ = None
    let runtime = Runtime.newRuntime host

    let ev = new ManualResetEvent(false)

    async {
        runtime.Run ivr |> should equal None
        ev.Set() |> ignore
    } |> Async.Start

    (runtime :> IDisposable).Dispose();

    ev.WaitOne(2000) |> ignore
        
    ct.disposed |> should equal true


[<Fact>]
let ``async can be used in ivrs``() = 

    let waitAndReturn10 = async {
        do! Async.Sleep(1)
        return 10
    }
        
    let test = ivr {
        let! v = IVR.await waitAndReturn10
        return v
    }

    let host _ _ = None

    let runtime = IVR.Runtime.newRuntime(host)
    let result = runtime.Run test
    result |> should equal (Some 10)


[<Fact>]
let ``async exceptions are propagated``() = 

    let waitAndReturn10 = async {
        do! Async.Sleep(1)
        raise (AsyncException "BOOM!")
        return 10
    }
        
    let test = ivr {
        let! v = IVR.await waitAndReturn10
        return v
    }

    let host _ _ = None

    let runtime = IVR.Runtime.newRuntime(host)
        
    (fun () -> runtime.Run test |> ignore)
    |> should throw typeof<AsyncException>

[<Fact>]
let ``async exceptions can be catched inside an IVR``() = 

    let waitAndReturn10 = async {
        do! Async.Sleep(1)
        raise (AsyncException "BOOM!")
        return 10
    }
        
    let test = ivr {
        try
            let! v = IVR.await waitAndReturn10
            return v
        with e ->
            return 11
    }

    let host _ _ = None

    let runtime = IVR.Runtime.newRuntime(host)
    runtime.Run test |> should equal (Some 11)
        

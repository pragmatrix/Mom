module Mom.Tests.RuntimeTests

open System
open System.Threading
open FsUnit
open Xunit
open Mom

exception AsyncException of string

type Request1 =
    | Request1
    interface Mom.IRequest<unit>

[<Fact>]
let ``host properly cancels its mom if the runtime gets disposed asynchronously``() =
    let ct = new MomTests.CancellationTracker()

    let mom = mom {
        use __ = ct
        do! Mom.waitFor' (fun _ -> true)
    }

    let host _ _ = None
    let runtime = Runtime.newRuntime host

    let ev = new ManualResetEvent(false)

    async {
        runtime.Run mom |> should equal None
        ev.Set() |> ignore
    } |> Async.Start

    (runtime :> IDisposable).Dispose();

    ev.WaitOne(2000) |> ignore
        
    ct.Disposed |> should equal true

[<Fact>]
let ``host errors are propagated inside the mom``() = 

    let host _ req = 
        failwith "Error"

    let mom = mom {
        try
            do! Mom.send Request1
            return false
        with e ->
            return true
    }

    let runtime = Runtime.newRuntime host
    let result = runtime.Run mom
    result |> should equal (Some true)

[<Fact>]
let ``async can be used in moms``() = 

    let waitAndReturn10 = async {
        do! Async.Sleep(1)
        return 10
    }
        
    let test = mom {
        let! v = Mom.await waitAndReturn10
        return v
    }

    let host _ _ = None

    let runtime = Mom.Runtime.newRuntime(host)
    let result = runtime.Run test
    result |> should equal (Some 10)


[<Fact>]
let ``async exceptions are propagated``() = 

    let waitAndReturn10 = async {
        do! Async.Sleep(1)
        raise (AsyncException "BOOM!")
        return 10
    }
        
    let test = mom {
        let! v = Mom.await waitAndReturn10
        return v
    }

    let host _ _ = None

    let runtime = Mom.Runtime.newRuntime(host)
        
    (fun () -> runtime.Run test |> ignore)
    |> should throw typeof<AsyncException>

[<Fact>]
let ``async exceptions can be catched inside an Mom``() = 

    let waitAndReturn10 = async {
        do! Async.Sleep(1)
        raise (AsyncException "BOOM!")
        return 10
    }
        
    let test = mom {
        try
            let! v = Mom.await waitAndReturn10
            return v
        with e ->
            return 11
    }

    let host _ _ = None

    let runtime = Mom.Runtime.newRuntime(host)
    runtime.Run test |> should equal (Some 11)

let private momThatThrows() = mom {
    failwith "error"
}
        
[<Fact>]
let ``stack traces of exceptions are preserved``() = 

    let host _ _ = 
        failwith "Error"

    let mom = momThatThrows()

    let runtime = Runtime.newRuntime host
    try
        runtime.Run mom |> ignore
        failwith "unexpected"
    with e ->
        let ex = string e
        printfn "%s" ex
        ex |> should haveSubstring "momThatThrows"

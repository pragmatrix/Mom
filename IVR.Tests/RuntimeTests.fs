namespace IVR.Tests

open NUnit.Framework
open FsUnit
open IVR

exception AsyncException of string

[<TestFixture>]
type RuntimeTests() = 

    [<Test>]
    member this.``async can be used in ivrs``() = 

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


    [<Test>]
    member this.``async exceptions are propagated``() = 

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

    [<Test>]
    member this.``async exceptions can be catched inside an IVR``() = 

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
        

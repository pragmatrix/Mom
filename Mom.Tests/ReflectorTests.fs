module Mom.Tests.ReflectorTests

open FsUnit
open Xunit
open Mom

let (^) = (<|)

[<Fact>]
let ``Reflector sends and receive a message``() =

    let sender, receiver = Reflector.create()
    
    let test = mom {
        // send a message
        do! sender(())
        // then receive it.
        do! receiver()
    }

    test
    |> Runtime.runCore
    |> should equal (Some ())

[<Fact>]
let ``Reflector sends and two parallel receiver receive it`` () =
    
    let sender, receiver = Reflector.create()
    let test = mom {
        // send a message
        do! sender(())
        // then receive it.
        let! _ = Mom.race [receiver(); receiver()]
        ()
    }

    // Expected to complete in one go.
    test
    |> Runtime.runCore
    |> should equal (Some ())

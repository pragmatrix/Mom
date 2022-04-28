module Mom.Tests.ReflectorTests

open FsUnit
open Xunit
open Mom

let (^) = (<|)

[<Fact>]
let ``Reflector sends and receive a message``() =
    
    let wrapped = Reflector.wrap ^ fun (sender, receiver) ->  mom {
        // send a message
        do! sender(())
        // then receive it.
        do! receiver()
    }

    // Expected to be run in one go.
    wrapped
    |> Mom.start
    |> Flux.isCompleted
    |> should be True




    



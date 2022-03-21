/// Tests inline requests
module Mom.Tests.InlineRequests

open System
open FsUnit
open Xunit
open Mom

type InlineRequest(value: int) =
    interface IInlineRequest<string, int> with
        override _.Execute(str: string) =
            (int str) + value

type InlineAsyncRequest(value: int) =
    interface IInlineAsyncRequest<string, int> with
        override _.Execute(str: string) = async {
            return (int str) + value
        }

[<Fact>]
let ``simple inline request with context``() =

    let context = "100"
    let runtime = 
        Runtime.newDefaultBuilder()
        |> Runtime.withService (InlineRequestService.create context)
        |> Runtime.build

    
    mom {
        let! r = InlineRequest(10)
        r |> should equal 110
        let! r = InlineAsyncRequest(11)
        r |> should equal 111
    }
    |> runtime.Run

namespace IVR.Tests

open NUnit.Framework
open FsUnit

open System
open IVR
open IVR.Host

//
// Basic Combinator and semantic tests.
//

type CancellationTracker() = 
    
    let mutable _disposed = false
    
    interface IDisposable with
        member this.Dispose() = _disposed <- true

    member this.disposed = _disposed;

type Event1 = Event1
type Event2 = Event2

[<TestFixture>]
type IVRTests() = 

    //
    // Automatic cancellation of Active ivrs.
    //

    [<Test>]
    member this.disposeIsCalledInASequentialIVR() = 
        let ct = new CancellationTracker()

        let a = ivr {
            use x = ct
            return 0
        }

        IVR.start a |> ignore
        ct.disposed |> should equal true


    [<Test>]
    member this.ivrIsCancelledInASequentialIVRSurroundingAWait() = 
        let ct = new CancellationTracker()

        let a = ivr {
            use x = ct
            do! IVR.waitFor' (fun (Event1) -> true)
            return 0
        }

        let started = IVR.start a
        ct.disposed |> should equal false
        IVR.step started Event1 |> ignore
        ct.disposed |> should equal true

    [<Test>]
    member this.``par': right ivr is cancelled after left completes``() = 
        let ct = new CancellationTracker()

        let left = ivr {
            do! IVR.waitFor' (fun (Event1) -> true)
        }

        let right = ivr {
            use ct = ct
            do! IVR.waitFor' (fun (Event2) -> true)
        }

        let test = IVR.par' left right
        let started = IVR.start test
        IVR.step started Event1 |> ignore
        ct.disposed |> should equal true

    [<Test>]
    member this.``par': left ivr is cancelled after right completes``() = 
        let ct = new CancellationTracker()

        let left = ivr {
            use ct = ct
            do! IVR.waitFor' (fun (Event1) -> true)
        }

        let right = ivr {
            do! IVR.waitFor' (fun (Event2) -> true)
        }

        let test = IVR.par' left right
        let started = IVR.start test
        IVR.step started Event2 |> ignore
        ct.disposed |> should equal true


    [<Test>]
    member this.``par': left ivr is cancelled when the right one is completed after startup``() = 
        let ct = new CancellationTracker()

        let left = ivr {
            use ct = ct
            do! IVR.waitFor' (fun (Event1) -> true)
        }

        let right = ivr {
            return 0
        }

        let test = IVR.par' left right
        let started = IVR.start test
        IVR.isCompleted started |> should equal true
        ct.disposed |> should equal true

    [<Test>]
    member this.FirstIVRIsCancelledInAParallelListWhenTheSecondOneFinishesFirst() = 
        let ct = new CancellationTracker()

        let left = ivr {
            use ct = ct
            do! IVR.waitFor' (fun (Event1) -> true)
        }

        let right = ivr {
            do! IVR.waitFor' (fun (Event2) -> true)
        }

        let r = IVR.lpar' [left; right]
        let started = IVR.start r
        IVR.step started Event2 |> ignore
        ct.disposed |> should equal true

    [<Test>]
    member this.SecondIVRIsCancelledInAParallelListWhenTheFirstOneFinishesFirst() = 
        let ct = new CancellationTracker()

        let left = ivr {
            do! IVR.waitFor' (fun (Event1) -> true)
        }

        let right = ivr {
            use ct = ct
            do! IVR.waitFor' (fun (Event2) -> true)
        }

        let r = IVR.lpar' [left; right]
        let started = IVR.start r
        IVR.step started Event1 |> ignore
        ct.disposed |> should equal true



    [<Test>]
    member this.hostProperlyCancelsTheIVRIfItsDisposed() =
        Assert.Fail()



    



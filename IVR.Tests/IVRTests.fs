module IVR.Tests.IVRTests

open System
open System.Collections.Generic
open FsUnit
open Xunit
open IVR

//
// Basic Combinator and semantic tests.
//

type CancellationTracker() = 
    
    let mutable _disposed = false
    
    interface IDisposable with
        member __.Dispose() = _disposed <- true

    member __.Disposed = _disposed;

type Event1 = Event1
type Event2 = Event2
type Event3 = Event3

type Request = 
    | Request of int
    interface IVR.IRequest<string>

type RequestU = 
    | RequestU of int
    interface IVR.IRequest<unit>

let start = IVR.start
let dispatch = Flux.dispatch
let stepH host ivr =
    match ivr with
    | Flux.Requesting (r, cont) -> 
        host r |> unbox |> IVR.Value |> cont
    | _ -> failwithf "stepH: invalid state %A" ivr


module Cancellation = 

    [<Fact>]
    let disposeIsCalledInASequentialIVR() = 
        let ct = new CancellationTracker()

        let a = ivr {
            use __ = ct
            return 0
        }

        start a |> Flux.resultValue |> should equal 0
        ct.Disposed |> should equal true

    [<Fact>]
    let disposableProcIsCalledAtStartupTime() = 

        let mutable trace = []
        let t x = 
            trace <- x::trace

        let x() = 
            ivr {
                t 0
                return
                    ivr { t 1 } |> IVR.asDisposable
            }

        let p = ivr {
            use! __ = x()
            t 2
            return ()
        }

        p 
        |> start
        |> ignore

        trace |> should equal [1;2;0]

    [<Fact>]
    let disposableProcIsCalledAfterWait() = 

        let mutable trace = []
        let t x = 
            trace <- x::trace

        let x() = 
            ivr {
                t 0
                do! IVR.waitFor' (fun (_:Event1) -> true)
                return
                    ivr { t 1 } |> IVR.asDisposable
            }

        let p = ivr {
            use! __ = x()
            t 2
            return ()
        }

        p 
        |> start
        |> dispatch Event1
        |> ignore

        trace |> should equal [1;2;0]

    [<Fact>]
    let ivrIsCancelledInASequentialIVRSurroundingAWait() = 
        let ct = new CancellationTracker()

        let a = ivr {
            use __ = ct
            do! IVR.waitFor' (fun (_:Event1) -> true)
            return 0
        }

        let started = start a
        ct.Disposed |> should equal false
        dispatch Event1 started |> ignore
        ct.Disposed |> should equal true

    [<Fact>]
    let ``join: right ivr is cancelled when left throws an exception``() = 
        let ct = new CancellationTracker()

        let left = ivr {
            do! IVR.waitFor' (fun (_:Event1) -> true)
            failwith "HellNo!"
        }

        let right = ivr {
            use __ = ct
            do! IVR.waitFor' (fun (_:Event2) -> true)
        }

        IVR.join left right
        |> start
        |> dispatch Event1 
        |> Flux.isError
        |> should equal true

        ct.Disposed |> should equal true

    [<Fact>]
    let ``par: left ivr is cancelled when right throws an exception``() = 
        let ct = new CancellationTracker()

        let left = ivr {
            use __ = ct
            do! IVR.waitFor' (fun (_:Event1) -> true)
        }

        let right = ivr {
            do! IVR.waitFor' (fun (_:Event2) -> true)
            failwith "HellNo!"
        }

        IVR.join left right
        |> start
        |> dispatch Event2
        |> Flux.isError
        |> should equal true

        ct.Disposed |> should equal true

    [<Fact>]
    let ``par': right ivr is cancelled after left completes``() = 
        let ct = new CancellationTracker()

        let left = ivr {
            do! IVR.waitFor' (fun (_:Event1) -> true)
        }

        let right = ivr {
            use __ = ct
            do! IVR.waitFor' (fun (_:Event2) -> true)
        }

        let test = IVR.first left right
        let started = start test
        dispatch Event1 started |> ignore
        ct.Disposed |> should equal true

    [<Fact>]
    let ``par': right ivr is cancelled after left completes and its finally handler is called``() = 
        let mutable finallyCalled = false

        let left = ivr {
            do! IVR.waitFor' (fun (_:Event1) -> true)
        }

        let right = ivr {
            try
                do! IVR.waitFor' (fun (_:Event2) -> true)
            finally
                finallyCalled <- true
        }

        let test = IVR.first left right
        let started = start test
        dispatch Event1 started |> ignore
        finallyCalled |> should equal true

    [<Fact>]
    let ``par': right ivr is cancelled after left completes and its finally ivr is run``() = 
        let mutable finallyCalled = false

        let leftResult = 1
        let rightResult = 2

        let left = ivr {
            do! IVR.waitFor' (fun (_:Event1) -> true)
            return leftResult
        }

        let right = ivr {
            try
                do! IVR.waitFor' (fun (_:Event2) -> true)
                return rightResult
            finally
                ivr {
                    do! IVR.waitFor' (fun (_:Event3) -> true)
                    finallyCalled <- true
                }
        }

        let test = IVR.first left right
        let result = 
            test
            |> start 
            |> dispatch Event1
            |> dispatch Event3
            
        result 
        |> Flux.resultValue 
        |> should equal (Choice<int,int>.Choice1Of2 leftResult)
        finallyCalled |> should equal true

    [<Fact>]
    let ``par': left ivr is cancelled after right completes``() = 
        let ct = new CancellationTracker()

        let left = ivr {
            use __ = ct
            do! IVR.waitFor' (fun (_:Event1) -> true)
        }

        let right = ivr {
            do! IVR.waitFor' (fun (_:Event2) -> true)
        }

        let test = IVR.first left right
        let started = start test
        dispatch Event2 started |> ignore
        ct.Disposed |> should equal true


    [<Fact>]
    let ``par': left ivr is cancelled when the right one is completed after startup``() = 
        let ct = new CancellationTracker()

        let left = ivr {
            use __ = ct
            do! IVR.waitFor' (fun (_:Event1) -> true)
        }

        let right = ivr {
            return 0
        }

        let test = IVR.first left right
        let started = start test
        Flux.isCompleted started |> should equal true
        ct.Disposed |> should equal true

    [<Fact>]
    let ``any: first ivr is cancelled when the second one finishes first``() = 
        let ct = new CancellationTracker()

        let left = ivr {
            use __ = ct
            do! IVR.waitFor' (fun (_:Event1) -> true)
        }

        let right = ivr {
            do! IVR.waitFor' (fun (_:Event2) -> true)
        }

        let r = IVR.any [left; right]
        let started = start r
        dispatch Event2 started |> ignore
        ct.Disposed |> should equal true

    [<Fact>]
    let ``any: second ivr is cancelled when the first one finishes first``() = 
        let ct = new CancellationTracker()

        let left = ivr {
            do! IVR.waitFor' (fun (_:Event1) -> true)
        }

        let right = ivr {
            use __ = ct
            do! IVR.waitFor' (fun (_:Event2) -> true)
        }

        let r = IVR.any [left; right]
        let started = start r
        dispatch Event1 started |> ignore
        ct.Disposed |> should equal true

    [<Fact>]
    let ``any: cancellation is done in reversed order specified``() = 

        let mutable finallyTracker = []

        let a = ivr {
            try
                do! IVR.waitFor' (fun (_:Event1) -> true)
            finally
                finallyTracker <- finallyTracker @ ['a']
            }

        let b = ivr {
            try
                do! IVR.waitFor' (fun (_:Event1) -> true)
            finally
                finallyTracker <- finallyTracker @ ['b']
            }

        let c = ivr {
            try
                do! IVR.waitFor' (fun (_:Event2) -> true)
            finally
                finallyTracker <- finallyTracker @ ['c']
            }

        let d = ivr {
            try
                do! IVR.waitFor' (fun (_:Event1) -> true)
            finally
                finallyTracker <- finallyTracker @ ['d']
            }

        let e = ivr {
            try
                do! IVR.waitFor' (fun (_:Event1) -> true)
            finally
                finallyTracker <- finallyTracker @ ['e']
            }

        let r = IVR.any [a;b;c;d;e]
        start r 
        |> dispatch Event2
        |> ignore

        printfn "%A" finallyTracker

        finallyTracker 
        |> should equal ['c';'e';'d';'b';'a']

    [<Fact>]
    let ``when an ivr gets cancelled, it ends cancelled but does not throw any exception``() = 

        let ivr = ivr {
            try
                do! IVR.waitFor' (fun (_:Event1) -> true)
            with _ ->
                failwith "fail"
        }

        let res = 
            ivr
            |> start
            |> dispatch Flux.TryCancel

        Flux.isCancelled res |> should be True

    [<Fact>]
    let ``when an ivr gets cancelled in a while loop, it does not continue outside of it``() = 

        let mutable continued = false

        let ivr = ivr {
            while true do
                do! IVR.waitFor' (fun (_:Event1) -> true)
            continued <- true
        }

        let res = 
            ivr
            |> start
            |> dispatch Flux.TryCancel
    
        Flux.isCancelled res |> should be True

module Finally =

    [<Fact>]
    let ``computation expression: try finally handler is run on a regular completion``() =
        let mutable x = false

        let test = ivr {
            try
                do! IVR.waitFor' (fun (_:Event1) -> true)
            finally
                x <- true
        }

        test
        |> start
        |> dispatch Event1
        |> ignore

        x |> should equal true

    [<Fact>]
    let ``computation expression: try finally handler is run on an error completion``() =
        let mutable x = false

        let test = ivr {
            try
                do! IVR.waitFor' (fun (_:Event1) -> true)
                failwith "Nooooo"
            finally
                x <- true
        }

        test
        |> start
        |> dispatch Event1
        |> ignore

        x |> should equal true

    [<Fact>]
    let ``computation expression: try finally handler is run on an error completion at startup time``() =
        let mutable x = false

        let test = ivr {
            try
                failwith "Nooooo"
            finally
                x <- true
        }

        test
        |> start
        |> ignore

        x |> should equal true

    [<Fact>]
    let ``computation expression: finally handler with ivr is called``() = 
        let mutable x = false

        let finallyIVR = ivr { x <- true }

        let test = ivr {
            try
                do! IVR.waitFor' (fun (_:Event1) -> true)
                return 10
            finally
                finallyIVR
            }

        test
        |> start
        |> dispatch Event1
        |> ignore

        x |> should equal true

    [<Fact>]
    let ``computation expression: finally handler with ivr is called when an exception happens``() = 
        let mutable x = false

        let finallyIVR = ivr { x <- true }

        let test = ivr {
            try
                do! IVR.waitFor' (fun (_:Event1) -> true)
                failwith "Nooooo"
                return 10
            finally
                finallyIVR
            }

        test
        |> start
        |> dispatch Event1
        |> ignore

        x |> should equal true

    [<Fact>]
    let ``computation expression: finally crashes``() = 
        let test = ivr {
            try
                return 10
            finally
                failwith "Nooo"
                ()
            }

        test
        |> start
        |> Flux.isError
        |> should equal true
        
    [<Fact>]
    let ``computation expression: finally ivr crashes``() = 
        let test = ivr {
            try
                return 10
            finally
                ivr { 
                    failwith "Nooo"
                }
            }

        test
        |> start
        |> Flux.isError
        |> should equal true
        
    [<Fact>]
    let ``computation expression: finally ivr crashes outside the ivr``() = 
        let test = ivr {
            try
                return 10
            finally
                failwith "Nooo"
                ivr { 
                    return ()
                }
            }

        test
        |> start
        |> Flux.isError
        |> should equal true

module Exceptions =

    [<Fact>]
    let ``handle exception at startup time``() =

        let test = ivr {
            try
                failwith "Nooooo"
                return 0
            with _ ->
                return 1
        }

        test
        |> start
        |> Flux.result
        |> should equal (IVR.Value 1)

    [<Fact>]
    let ``handles exception at runtime``() =

        let test = ivr {
            try
                do! IVR.waitFor' (fun (_:Event1) -> true)
                failwith "Nooooo"
                return 0
            with _ ->
                return 1
        }

        test
        |> start
        |> dispatch Event1
        |> Flux.result
        |> should equal (IVR.Value 1)

    [<Fact>]
    let ``a waiter's exception is captured``() =

        let msg = "Waiter Crashed"
        let test = ivr {
            try
                do! IVR.waitFor' (fun (_:Event1) -> failwith msg)
                return 0
            with e ->
                e.Message |> should equal msg
                return 1
        }

        test
        |> start
        |> dispatch Event1
        |> Flux.result
        |> should equal (IVR.Value 1)

    [<Fact>]
    let ``handle exception in exception handler after wait``() =

        let test = ivr {
            try
                do! IVR.waitFor' (fun (_:Event1) -> true)
                failwith "Nooooo"
                return 0
            with _ ->
                failwith "AGAIN"
                return 1
        }

        let res = 
            test
            |> start
            |> dispatch Event1
        
        res |> Flux.isError |> should be True

        res 
        |> Flux.result
        |> sprintf "%A"
        |> should haveSubstring "AGAIN"

module CancellationAndFinally =

    [<Fact>]
    let ``when an ivr gets cancelled, it runs the finally handler``() = 

        let mutable runFinally = false

        let ivr = ivr {
            try
                do! IVR.waitFor' (fun (_:Event1) -> true)
            finally
                runFinally <- true
        }

        let res = 
            ivr
            |> start
            |> dispatch Flux.TryCancel
    
        runFinally |> should be True
        Flux.isCancelled res |> should be True

    [<Fact>]
    let ``cancelled ivrs may propagate errors``() =

        let ivr = ivr {
            try
                do! IVR.waitFor' (fun (_:Event1) -> true)
            finally
                failwith "error" |> ignore
        }

        let res = 
            ivr
            |> start
            |> dispatch Flux.TryCancel

        Flux.isError res |> should be True

module UnresolvedIssues =
        
    [<Fact(Skip="See issue #4")>]
    let ``when one part of a parallel ivr gets cancelled, the followup ivr continues while the cancellation is run``() = 

        let primaryIVR = ivr {
                do! IVR.waitFor' (fun (_:Event1) -> true)
            }

        // cancellation of the secondary IVR takes forvever
        let secondaryIVR = ivr {
            try
                do! IVR.waitFor' (fun (_:Event2) -> true)
            finally
                IVR.idle
        }

        let test = ivr {
                do! IVR.any [primaryIVR; secondaryIVR]
                do! IVR.waitFor' (fun (_:Event3) -> true)
            }

        let res = 
            test
            |> start
            |> dispatch Event1
            |> dispatch Event3

        res |> Flux.isCompleted |> should be True        

module HostCommunication =
    [<Fact>]
    let ``IVR.send sends a request to the host``() =
        let queue = Queue()
        
        let test = ivr {
            do! IVR.send (RequestU 0)
        } 

        test
        |> start
        |> stepH (fun v -> queue.Enqueue v; null)
        |> ignore

        queue |> Seq.toList |> should equal [box (RequestU 0)]


    [<Fact>]
    let ``IVR.send sends requests to the host in the right order``() =
        let queue = Queue()
        
        let test = ivr {
            do! IVR.send (RequestU 0)
            do! IVR.send (RequestU 1)
        } 

        test
        |> start
        |> stepH queue.Enqueue
        |> stepH queue.Enqueue
        |> ignore

        queue |> Seq.toList |> should equal [box <| RequestU 0; box <| RequestU 1]

    [<Fact>]
    let ``after a wait, IVR.send sends a request to the host``() =
        let queue = Queue()
        
        let test = ivr {
            do! IVR.send (RequestU 0)
            do! IVR.waitFor' (fun (_:Event1) -> true)
            do! IVR.send (RequestU 1)
        } 

        test
        |> start
        |> stepH queue.Enqueue
        |> dispatch Event1
        |> stepH queue.Enqueue
        |> ignore

        queue |> Seq.toList |> should equal [box <| RequestU 0; box <| RequestU 1]

module Delay =

    [<Fact>]
    let ``IVR.delay (simulated)``() = 

        let expectDelay (c: obj) = 
            c |> should equal (IVR.Delay (TimeSpan(1, 0, 0)))
            Id 1L |> box // return the 64 bit id of the Delay

        let expectCancelDelay (c: obj) = 
            c |> should equal (IVR.CancelDelay (Id 1L))
            () |> box

        let test = IVR.delay (TimeSpan(1, 0, 0))

        test
        |> start
        |> stepH expectDelay
        |> dispatch (IVR.DelayCompleted (Id 1L))
        |> stepH expectCancelDelay
        |> Flux.isCompleted |> should equal true

    [<Fact>]
    let ``IVR.delay completes immediately in case of a zero delay``() = 

        let test = IVR.delay TimeSpan.Zero

        let state = 
            test |> start
            
        state 
        |> Flux.isCompleted |> should be True

module CompuationExpressionSyntax =

    [<Fact>]
    let ``Request with response type``() = 
        let test = ivr {
            let! r = IVR.send (Request 10)
            return r
        }

        let host (c:obj) : obj =
            match box c with
            | :? Request -> "Hello"
            | _ -> ""
            |> box

        test
        |> start
        |> stepH host
        |> Flux.resultValue
        |> should equal "Hello"

    [<Fact>]
    let ``Command with return type (implicit send!)``() = 
        let test = ivr {
            let! r = Request 10
            return r
        }

        let host (c:obj) : obj =
            match box c with
            | :? Request -> "Hello"
            | _ -> ""
            |> box

        test
        |> start
        |> stepH host
        |> Flux.resultValue
        |> should equal "Hello"
        
    [<Fact>]
    let ``while``() = 
        let test = ivr {
            let mutable f = 0
            while f<3 do
                do! IVR.waitFor' (fun (_:Event1) -> true)
                f <- f + 1
            return f
        }

        test
        |> start
        |> dispatch Event1
        |> dispatch Event1
        |> dispatch Event1
        |> Flux.result
        |> should equal (IVR.Value 3) 

    [<Fact>]
    let ``For``() = 
        let test = ivr {
            for _ in [0..2] do
                do! IVR.waitFor' (fun (_:Event1) -> true)
        }

        test
        |> start
        |> dispatch Event1
        |> dispatch Event1
        |> dispatch Event1
        |> Flux.result
        |> should equal (IVR.Value ()) 

module AsyncRequest = 

    type AsyncRequest =
        | AsyncRequest
        interface IVR.IAsyncRequest<string>
    
    [<Fact>]
    let ``AsyncRequest works``() = 
        let test = ivr {
            let! r = AsyncRequest
            return r
        }

        let host (c:obj) : obj =
            match c with
            | :? AsyncRequest ->
                Id 10L |> box
            | _ -> () |> box

        test
        |> start
        |> stepH host
        |> dispatch (IVR.AsyncResponse(Id 10L, IVR.Value "Hello"))
        |> Flux.result
        |> should equal (IVR.Value "Hello")

    [<Fact>]
    let ``AsyncRequest cancellation works``() = 
        let mutable cancelled = false
        let test = ivr {
            try
                let! _ = AsyncRequest
                return ""
            finally
                cancelled <- true
        }

        let host (c:obj) : obj =
            match c with
            | :? AsyncRequest ->
                Id 10L |> box
            | _ -> () |> box

        test
        |> start
        |> stepH host
        |> dispatch (IVR.AsyncResponse(Id 10L, IVR.Cancelled) : IVR.AsyncResponse<string>)
        |> Flux.result
        |> should equal (IVR.Cancelled : string IVR.result)

        cancelled |> should be True

    [<Fact>]
    let ``AsyncRequest exceptions can be catched``() = 
        let test = ivr {
            try
                let! _ = AsyncRequest
                return ""
            with :? InvalidOperationException ->
                return "Catched"
        }

        let host (c:obj) : obj =
            match c with
            | :? AsyncRequest ->
                Id 10L |> box
            | _ -> () |> box

        test
        |> start
        |> stepH host
        |> dispatch (IVR.AsyncResponse(Id 10L, IVR.Error (InvalidOperationException())) : IVR.AsyncResponse<string>)
        |> Flux.result
        |> should equal (IVR.Value "Catched")
     
module Arbiter = 
    
    [<Fact>]
    let immediateContinuationIsRunOnBothPathsInAParallelIVR() = 
        let mutable r1 = false
    
        let ivr1 = ivr {
            do! IVR.waitFor' (fun (_:Event1) -> true)
            r1 <- true
        }

        let mutable r2 = false

        let ivr2 = ivr {
            do! IVR.waitFor' (fun (_:Event1) -> true)
            r2 <- true
        }

        IVR.all [ivr1;ivr2]
        |> start
        |> dispatch Event1
        |> Flux.resultValue
        |> should equal [();()]
        
        r1 |> should be True
        r2 |> should be True

    [<Fact>]
    let hostContinuationIsRunOnBothPathsInAParallelIVR() = 

        let queue = Queue()
    
        let ivr1 = ivr {
            do! IVR.waitFor' (fun (_:Event1) -> true)
            do! IVR.send (RequestU 0)
        }

        let ivr2 = ivr {
            do! IVR.waitFor' (fun (_:Event1) -> true)
            do! IVR.send (RequestU 1)
        }

        IVR.all [ivr1;ivr2]
        |> start
        |> dispatch Event1
        |> stepH queue.Enqueue
        |> stepH queue.Enqueue
        |> Flux.resultValue
        |> should equal [();()]

        queue |> Seq.toList |> should equal [box (RequestU 0);box (RequestU 1)]
        
module Sideshow =

    [<Fact>]
    let goodCase() =

        // this tests replacement while the sideshow is running.
    
        let mutable sideshowStarted = 0
        let mutable nestedContinued = 0
        let mutable sideshowFinalized = 0

        let sideshow = ivr {
            try
                sideshowStarted <- sideshowStarted + 1
                do! IVR.waitFor' (fun (_:Event2) -> true)
            finally
                sideshowFinalized <- sideshowFinalized + 1
        }

        let control (control: Sideshow.Control) = ivr {
            do! IVR.waitFor' (fun (_:Event1) -> true)
            do! control.Begin sideshow
            nestedContinued <- nestedContinued + 1
            do! IVR.waitFor' (fun (_:Event1) -> true)
            do! control.Begin sideshow
            nestedContinued <- nestedContinued + 1
            do! IVR.waitFor' (fun (_:Event1) -> true)
        }

        let state =
            Sideshow.attachTo control
            |> start
            |> dispatch Event1
        
        (sideshowStarted, nestedContinued, sideshowFinalized) |> should equal (1, 1, 0)

        let state =
            state
            |> dispatch Event1

        (sideshowStarted, nestedContinued, sideshowFinalized) |> should equal (2, 2, 1)

        let state = 
            state
            |> dispatch Event2

        (sideshowStarted, nestedContinued, sideshowFinalized) |> should equal (2, 2, 2)

    module ErrorPropagation =

        [<Fact>]
        let ``sideshow error while starting is propagated into the control ivr``() = 

            // we start it, we control it!
        
            let sideshow = ivr {
                failwith "error"
            }

            let control (control: Sideshow.Control) = ivr {
                try
                    do! control.Begin sideshow
                    return false
                with e ->
                    return true
            }

            let state =
                Sideshow.attachTo control |> start

            Flux.resultValue state |> should equal true

        [<Fact>]
        let ``sideshow error after an event is propagated to a subsequent replace``() = 

            let sideshow = ivr {
                do! IVR.waitFor' (fun (_:Event1) -> true)
                failwith "error" |> ignore
            }

            let control (control: Sideshow.Control) = ivr {
                do! control.Begin sideshow
                do! IVR.waitFor' (fun (_:Event1) -> true)
                try
                    do! control.Begin sideshow
                    return false
                with e ->
                    return true
            }

            let state =
                Sideshow.attachTo control |> start
                |> dispatch Event1

            Flux.resultValue state |> should equal true


        [<Fact>]
        let ``sideshow error while cancelling is propagated in a subsequent replace``() = 

            let sideshow = ivr {
                try
                    do! IVR.waitFor' (fun (_:Event2) -> true)
                finally
                    failwith "error" |> ignore
            }

            let control (control: Sideshow.Control) = ivr {
                do! control.Begin sideshow
                try
                    do! control.Begin sideshow
                    return false
                with e ->
                    return true
            }

            let state =
                Sideshow.attachTo control |> start

            Flux.resultValue state |> should equal true

        [<Fact>]
        let ``pending sideshow error overrides successful value``() = 

            let sideshow = ivr {
                do! IVR.waitFor' (fun (_:Event1) -> true)
                failwith "error" |> ignore
            }

            let control (control: Sideshow.Control) = ivr {
                do! control.Begin sideshow
                do! IVR.waitFor' (fun (_:Event1) -> true)
            }

            let state =
                Sideshow.attachTo control |> start
                |> dispatch Event1

            Flux.isError state |> should equal true

        [<Fact>]
        let ``pending sideshow cancellation error overrides successful value``() = 

            let sideshow = ivr {
                try
                    do! IVR.waitFor' (fun (_:Event1) -> true)
                finally
                    failwith "error" |> ignore
            }

            let control (control: Sideshow.Control) = ivr {
                do! control.Begin sideshow
            }

            let state =
                Sideshow.attachTo control |> start

            Flux.isError state |> should equal true

        [<Fact>]
        let ``if both sideshow and control are in error, the control error has precedence``() = 
            
            // the nested error is the control ivr for the sideshow ivr, so if there happens
            // and error there, it should be considered as more important.
            
            let sideshow = ivr {
                try
                    do! IVR.waitFor' (fun (_:Event1) -> true)
                finally
                    failwith "error-sideshow" |> ignore
            }

            let control (control: Sideshow.Control) = ivr {
                do! control.Begin sideshow
                failwith "error-control"
            }

            let state =
                Sideshow.attachTo control |> start

            (Flux.error state).Message |> should equal "error-control"

    module State = 
        [<Fact>] 
        let ``state is initially set to None``() = 
            let control (control: Sideshow.Control) = ivr {
                return! control.State
            }

            let state =
                Sideshow.attachTo control |> start

            state
            |> Flux.resultValue
            |> should equal None

        [<Fact>]
        let ``if sideshow terminates immediately, state stays at None``() = 

            let sideshow = ivr {
                return ()
            }

            let control (control: Sideshow.Control) = ivr {
                do! control.Begin(sideshow)
                return! control.State
            }

            let state =
                Sideshow.attachTo control |> start

            state
            |> Flux.resultValue
            |> should equal None

        [<Fact>]
        let ``if sideshow takes time , state is set ``() = 

            let sideshow = IVR.idle

            let control (control: Sideshow.Control) = ivr {
                do! control.Begin(sideshow)
                return! control.State
            }

            let state =
                Sideshow.attachTo control |> start

            state
            |> Flux.resultValue
            |> should equal (Some ())

        [<Fact>]
        let ``sideshow state changes``() = 

            let sideshow = IVR.idle
            let sideshowNone = IVR.ofValue ()

            let control (control: Sideshow.Control<int>) = ivr {
                do! control.Begin(1, sideshow)
                let! state1 = control.State
                do! control.Begin(2, sideshow)
                let! state2 = control.State
                do! control.Begin(3, sideshowNone)
                let! state3 = control.State
                return state1, state2, state3
            }

            let state =
                Sideshow.attachTo control |> start

            let noneInt: Option<int> = None

            state
            |> Flux.resultValue
            |> should equal (Some 1, Some 2, noneInt)

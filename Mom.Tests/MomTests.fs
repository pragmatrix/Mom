module Mom.Tests.MomTests

open System
open System.Collections.Generic
open FsUnit
open Xunit
open Mom

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
    interface Mom.IRequest<string>

type RequestU = 
    | RequestU of int
    interface Mom.IRequest<unit>

let start = Mom.start
let dispatch = Flux.dispatch
let stepH host mom =
    match mom with
    | Flux.Requesting (r, cont) -> 
        host r |> unbox |> Flux.Value |> cont
    | _ -> failwithf "stepH: invalid state %A" mom


module Cancellation = 

    [<Fact>]
    let disposeIsCalledInASequentialMom() = 
        let ct = new CancellationTracker()

        let a = mom {
            use __ = ct
            return 0
        }

        start a |> Flux.value |> should equal 0
        ct.Disposed |> should equal true

    [<Fact>]
    let disposableProcIsCalledAtStartupTime() = 

        let mutable trace = []
        let t x = 
            trace <- x::trace

        let x() = 
            mom {
                t 0
                return
                    mom { t 1 } |> Mom.asDisposable
            }

        let p = mom {
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
            mom {
                t 0
                do! Mom.waitFor' (fun (_:Event1) -> true)
                return
                    mom { t 1 } |> Mom.asDisposable
            }

        let p = mom {
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
    let momIsCancelledInASequentialMomSurroundingAWait() = 
        let ct = new CancellationTracker()

        let a = mom {
            use __ = ct
            do! Mom.waitFor' (fun (_:Event1) -> true)
            return 0
        }

        let started = start a
        ct.Disposed |> should equal false
        dispatch Event1 started |> ignore
        ct.Disposed |> should equal true

    [<Fact>]
    let ``join: right mom is cancelled when left throws an exception``() = 
        let ct = new CancellationTracker()

        let left = mom {
            do! Mom.waitFor' (fun (_:Event1) -> true)
            failwith "HellNo!"
        }

        let right = mom {
            use __ = ct
            do! Mom.waitFor' (fun (_:Event2) -> true)
        }

        Mom.join left right
        |> start
        |> dispatch Event1 
        |> Flux.isError
        |> should equal true

        ct.Disposed |> should equal true

    [<Fact>]
    let ``par: left mom is cancelled when right throws an exception``() = 
        let ct = new CancellationTracker()

        let left = mom {
            use __ = ct
            do! Mom.waitFor' (fun (_:Event1) -> true)
        }

        let right = mom {
            do! Mom.waitFor' (fun (_:Event2) -> true)
            failwith "HellNo!"
        }

        Mom.join left right
        |> start
        |> dispatch Event2
        |> Flux.isError
        |> should equal true

        ct.Disposed |> should equal true

    [<Fact>]
    let ``par': right mom is cancelled after left completes``() = 
        let ct = new CancellationTracker()

        let left = mom {
            do! Mom.waitFor' (fun (_:Event1) -> true)
        }

        let right = mom {
            use __ = ct
            do! Mom.waitFor' (fun (_:Event2) -> true)
        }

        let test = Mom.first left right
        let started = start test
        dispatch Event1 started |> ignore
        ct.Disposed |> should equal true

    [<Fact>]
    let ``par': right mom is cancelled after left completes and its finally handler is called``() = 
        let mutable finallyCalled = false

        let left = mom {
            do! Mom.waitFor' (fun (_:Event1) -> true)
        }

        let right = mom {
            try
                do! Mom.waitFor' (fun (_:Event2) -> true)
            finally
                finallyCalled <- true
        }

        let test = Mom.first left right
        let started = start test
        dispatch Event1 started |> ignore
        finallyCalled |> should equal true

#if false

    // This test is currently not possible, because of #13. 

    [<Fact>]
    let ``par': right mom is cancelled after left completes and its finally mom is run``() = 
        let mutable finallyCalled = false

        let leftResult = 1
        let rightResult = 2

        let left = mom {
            do! Mom.waitFor' (fun (_:Event1) -> true)
            return leftResult
        }

        let right = mom {
            try
                do! Mom.waitFor' (fun (_:Event2) -> true)
                return rightResult
            finally
                mom {
                    do! Mom.waitFor' (fun (_:Event3) -> true)
                    finallyCalled <- true
                }
        }

        let test = Mom.first left right
        let result = 
            test
            |> start 
            |> dispatch Event1
            |> dispatch Event3
            
        result 
        |> Flux.value 
        |> should equal (Choice<int,int>.Choice1Of2 leftResult)
        finallyCalled |> should equal true

#endif

    [<Fact>]
    let ``par': left mom is cancelled after right completes``() = 
        let ct = new CancellationTracker()

        let left = mom {
            use __ = ct
            do! Mom.waitFor' (fun (_:Event1) -> true)
        }

        let right = mom {
            do! Mom.waitFor' (fun (_:Event2) -> true)
        }

        let test = Mom.first left right
        let started = start test
        dispatch Event2 started |> ignore
        ct.Disposed |> should equal true


    [<Fact>]
    let ``par': left mom is cancelled when the right one is completed after startup``() = 
        let ct = new CancellationTracker()

        let left = mom {
            use __ = ct
            do! Mom.waitFor' (fun (_:Event1) -> true)
        }

        let right = mom {
            return 0
        }

        let test = Mom.first left right
        let started = start test
        Flux.isCompleted started |> should equal true
        ct.Disposed |> should equal true

    [<Fact>]
    let ``any: first mom is cancelled when the second one finishes first``() = 
        let ct = new CancellationTracker()

        let left = mom {
            use __ = ct
            do! Mom.waitFor' (fun (_:Event1) -> true)
        }

        let right = mom {
            do! Mom.waitFor' (fun (_:Event2) -> true)
        }

        let r = Mom.any [left; right]
        let started = start r
        dispatch Event2 started |> ignore
        ct.Disposed |> should equal true

    [<Fact>]
    let ``any: second mom is cancelled when the first one finishes first``() = 
        let ct = new CancellationTracker()

        let left = mom {
            do! Mom.waitFor' (fun (_:Event1) -> true)
        }

        let right = mom {
            use __ = ct
            do! Mom.waitFor' (fun (_:Event2) -> true)
        }

        let r = Mom.any [left; right]
        let started = start r
        dispatch Event1 started |> ignore
        ct.Disposed |> should equal true

    [<Fact>]
    let ``any: cancellation is done in reversed order specified``() = 

        let mutable finallyTracker = []

        let a = mom {
            try
                do! Mom.waitFor' (fun (_:Event1) -> true)
            finally
                finallyTracker <- finallyTracker @ ['a']
            }

        let b = mom {
            try
                do! Mom.waitFor' (fun (_:Event1) -> true)
            finally
                finallyTracker <- finallyTracker @ ['b']
            }

        let c = mom {
            try
                do! Mom.waitFor' (fun (_:Event2) -> true)
            finally
                finallyTracker <- finallyTracker @ ['c']
            }

        let d = mom {
            try
                do! Mom.waitFor' (fun (_:Event1) -> true)
            finally
                finallyTracker <- finallyTracker @ ['d']
            }

        let e = mom {
            try
                do! Mom.waitFor' (fun (_:Event1) -> true)
            finally
                finallyTracker <- finallyTracker @ ['e']
            }

        let r = Mom.any [a;b;c;d;e]
        start r 
        |> dispatch Event2
        |> ignore

        printfn "%A" finallyTracker

        finallyTracker 
        |> should equal ['c';'e';'d';'b';'a']

    [<Fact>]
    let ``when an mom gets cancelled, it ends cancelled but does not throw any exception``() = 

        let mom = mom {
            try
                do! Mom.waitFor' (fun (_:Event1) -> true)
            with _ ->
                failwith "fail"
        }

        let res = 
            mom
            |> start
            |> dispatch Flux.Cancel

        Flux.isCancelled res |> should be True

    [<Fact>]
    let ``when an mom gets cancelled in a while loop, it does not continue outside of it``() = 

        let mutable continued = false

        let mom = mom {
            while true do
                do! Mom.waitFor' (fun (_:Event1) -> true)
            continued <- true
        }

        let res = 
            mom
            |> start
            |> dispatch Flux.Cancel
    
        Flux.isCancelled res |> should be True

module Finally =

    [<Fact>]
    let ``computation expression: try finally handler is run on a regular completion``() =
        let mutable x = false

        let test = mom {
            try
                do! Mom.waitFor' (fun (_:Event1) -> true)
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

        let test = mom {
            try
                do! Mom.waitFor' (fun (_:Event1) -> true)
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

        let test = mom {
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
    let ``computation expression: finally handler with mom is called``() = 
        let mutable x = false

        let finallyMom = mom { x <- true }

        let test = mom {
            try
                do! Mom.waitFor' (fun (_:Event1) -> true)
                return 10
            finally
                finallyMom
            }

        test
        |> start
        |> dispatch Event1
        |> ignore

        x |> should equal true

    [<Fact>]
    let ``computation expression: finally handler with mom is called when an exception happens``() = 
        let mutable x = false

        let finallyMom = mom { x <- true }

        let test = mom {
            try
                do! Mom.waitFor' (fun (_:Event1) -> true)
                failwith "Nooooo"
                return 10
            finally
                finallyMom
            }

        test
        |> start
        |> dispatch Event1
        |> ignore

        x |> should equal true

    [<Fact>]
    let ``computation expression: finally crashes``() = 
        let test = mom {
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
    let ``computation expression: finally mom crashes``() = 
        let test = mom {
            try
                return 10
            finally
                mom { 
                    failwith "Nooo"
                }
            }

        test
        |> start
        |> Flux.isError
        |> should equal true
        
    [<Fact>]
    let ``computation expression: finally mom crashes outside the mom``() = 
        let test = mom {
            try
                return 10
            finally
                failwith "Nooo"
                mom { 
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

        let test = mom {
            try
                failwith "Nooooo"
                return 0
            with _ ->
                return 1
        }

        test
        |> start
        |> Flux.result
        |> should equal (Flux.Value 1)

    [<Fact>]
    let ``handles exception at runtime``() =

        let test = mom {
            try
                do! Mom.waitFor' (fun (_:Event1) -> true)
                failwith "Nooooo"
                return 0
            with _ ->
                return 1
        }

        test
        |> start
        |> dispatch Event1
        |> Flux.result
        |> should equal (Flux.Value 1)

    [<Fact>]
    let ``a waiter's exception is captured``() =

        let msg = "Waiter Crashed"
        let test = mom {
            try
                do! Mom.waitFor' (fun (_:Event1) -> failwith msg)
                return 0
            with e ->
                e.Message |> should equal msg
                return 1
        }

        test
        |> start
        |> dispatch Event1
        |> Flux.result
        |> should equal (Flux.Value 1)

    [<Fact>]
    let ``handle exception in exception handler after wait``() =

        let test = mom {
            try
                do! Mom.waitFor' (fun (_:Event1) -> true)
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
    let ``when an mom gets cancelled, it runs the finally handler``() = 

        let mutable runFinally = false

        let mom = mom {
            try
                do! Mom.waitFor' (fun (_:Event1) -> true)
            finally
                runFinally <- true
        }

        let res = 
            mom
            |> start
            |> dispatch Flux.Cancel
    
        runFinally |> should be True
        Flux.isCancelled res |> should be True

    [<Fact>]
    let ``cancelled moms may propagate errors``() =

        let mom = mom {
            try
                do! Mom.waitFor' (fun (_:Event1) -> true)
            finally
                failwith "error" |> ignore
        }

        let res = 
            mom
            |> start
            |> dispatch Flux.Cancel

        Flux.isError res |> should be True

// testing for asynchronous finally handlers is currently done in DEBUG builds only
#if DEBUG 

module Synchronous =

    [<Fact>]
    let ``asynchronous finally handler leads to an error``() =

        let mom = mom {
            try
                ()
            finally
                mom { do! Mom.waitFor' (fun (_: Event1) -> true) }
        }

        let res = 
            mom
            |> start

        Flux.error res |> should equal (Mom.AsynchronousException("finally"))

#endif

module UnresolvedIssues =
        
    [<Fact(Skip="See issue #4")>]
    let ``when one part of a parallel mom gets cancelled, the followup mom continues while the cancellation is run``() = 

        let primaryMom = mom {
                do! Mom.waitFor' (fun (_:Event1) -> true)
            }

        // cancellation of the secondary Mom takes forvever
        let secondaryMom = mom {
            try
                do! Mom.waitFor' (fun (_:Event2) -> true)
            finally
                Mom.idle
        }

        let test = mom {
                do! Mom.any [primaryMom; secondaryMom]
                do! Mom.waitFor' (fun (_:Event3) -> true)
            }

        let res = 
            test
            |> start
            |> dispatch Event1
            |> dispatch Event3

        res |> Flux.isCompleted |> should be True        

module HostCommunication =
    [<Fact>]
    let ``Mom.send sends a request to the host``() =
        let queue = Queue()
        
        let test = mom {
            do! Mom.send (RequestU 0)
        } 

        test
        |> start
        |> stepH (fun v -> queue.Enqueue v; null)
        |> ignore

        queue |> Seq.toList |> should equal [box (RequestU 0)]


    [<Fact>]
    let ``Mom.send sends requests to the host in the right order``() =
        let queue = Queue()
        
        let test = mom {
            do! Mom.send (RequestU 0)
            do! Mom.send (RequestU 1)
        } 

        test
        |> start
        |> stepH queue.Enqueue
        |> stepH queue.Enqueue
        |> ignore

        queue |> Seq.toList |> should equal [box <| RequestU 0; box <| RequestU 1]

    [<Fact>]
    let ``after a wait, Mom.send sends a request to the host``() =
        let queue = Queue()
        
        let test = mom {
            do! Mom.send (RequestU 0)
            do! Mom.waitFor' (fun (_:Event1) -> true)
            do! Mom.send (RequestU 1)
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
    let ``Mom.delay (simulated)``() = 

        let expectDelay (c: obj) = 
            c |> should equal (Mom.Delay (TimeSpan(1, 0, 0)))
            Id 1L |> box // return the 64 bit id of the Delay

        let expectCancelDelay (c: obj) = 
            c |> should equal (Mom.CancelDelay (Id 1L))
            () |> box

        let test = Mom.delay (TimeSpan(1, 0, 0))

        test
        |> start
        |> stepH expectDelay
        |> dispatch (Mom.DelayCompleted (Id 1L))
        |> stepH expectCancelDelay
        |> Flux.isCompleted |> should equal true

    [<Fact>]
    let ``Mom.delay completes immediately in case of a zero delay``() = 

        let test = Mom.delay TimeSpan.Zero

        let state = 
            test |> start
            
        state 
        |> Flux.isCompleted |> should be True

module CompuationExpressionSyntax =

    [<Fact>]
    let ``Request with response type``() = 
        let test = mom {
            let! r = Mom.send (Request 10)
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
        |> Flux.value
        |> should equal "Hello"

    [<Fact>]
    let ``Command with return type (implicit send!)``() = 
        let test = mom {
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
        |> Flux.value
        |> should equal "Hello"
        
    [<Fact>]
    let ``while``() = 
        let test = mom {
            let mutable f = 0
            while f<3 do
                do! Mom.waitFor' (fun (_:Event1) -> true)
                f <- f + 1
            return f
        }

        test
        |> start
        |> dispatch Event1
        |> dispatch Event1
        |> dispatch Event1
        |> Flux.result
        |> should equal (Flux.Value 3) 

    [<Fact>]
    let ``For``() = 
        let test = mom {
            for _ in [0..2] do
                do! Mom.waitFor' (fun (_:Event1) -> true)
        }

        test
        |> start
        |> dispatch Event1
        |> dispatch Event1
        |> dispatch Event1
        |> Flux.result
        |> should equal (Flux.Value ()) 

module AsyncRequest = 

    type AsyncRequest =
        | AsyncRequest
        interface Mom.IAsyncRequest<string>
    
    [<Fact>]
    let ``AsyncRequest works``() = 
        let test = mom {
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
        |> dispatch (Mom.AsyncResponse(Id 10L, Flux.Value "Hello"))
        |> Flux.result
        |> should equal (Flux.Value "Hello")

    [<Fact>]
    let ``AsyncRequest cancellation works``() = 
        let mutable cancelled = false
        let test = mom {
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
        |> dispatch (Mom.AsyncResponse(Id 10L, Flux.Cancelled) : Mom.AsyncResponse<string>)
        |> Flux.result
        |> should equal (Flux.Cancelled : string Flux.result)

        cancelled |> should be True

    [<Fact>]
    let ``AsyncRequest exceptions can be catched``() = 
        let test = mom {
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
        |> dispatch (Mom.AsyncResponse(Id 10L, Flux.Error (InvalidOperationException())) : Mom.AsyncResponse<string>)
        |> Flux.result
        |> should equal (Flux.Value "Catched")
     
module Arbiter = 
    
    [<Fact>]
    let immediateContinuationIsRunOnBothPathsInAParallelMom() = 
        let mutable r1 = false
    
        let mom1 = mom {
            do! Mom.waitFor' (fun (_:Event1) -> true)
            r1 <- true
        }

        let mutable r2 = false

        let mom2 = mom {
            do! Mom.waitFor' (fun (_:Event1) -> true)
            r2 <- true
        }

        Mom.all [mom1;mom2]
        |> start
        |> dispatch Event1
        |> Flux.value
        |> should equal [();()]
        
        r1 |> should be True
        r2 |> should be True

    [<Fact>]
    let hostContinuationIsRunOnBothPathsInAParallelMom() = 

        let queue = Queue()
    
        let mom1 = mom {
            do! Mom.waitFor' (fun (_:Event1) -> true)
            do! Mom.send (RequestU 0)
        }

        let mom2 = mom {
            do! Mom.waitFor' (fun (_:Event1) -> true)
            do! Mom.send (RequestU 1)
        }

        Mom.all [mom1;mom2]
        |> start
        |> dispatch Event1
        |> stepH queue.Enqueue
        |> stepH queue.Enqueue
        |> Flux.value
        |> should equal [();()]

        queue |> Seq.toList |> should equal [box (RequestU 0);box (RequestU 1)]
        
module Sideshow =

    [<Fact>]
    let goodCase() =

        // this tests replacement while the sideshow is running.
    
        let mutable sideshowStarted = 0
        let mutable nestedContinued = 0
        let mutable sideshowFinalized = 0

        let sideshow = mom {
            try
                sideshowStarted <- sideshowStarted + 1
                do! Mom.waitFor' (fun (_:Event2) -> true)
            finally
                sideshowFinalized <- sideshowFinalized + 1
        }

        let control (control: Sideshow.Control) = mom {
            do! Mom.waitFor' (fun (_:Event1) -> true)
            do! control.Begin sideshow
            nestedContinued <- nestedContinued + 1
            do! Mom.waitFor' (fun (_:Event1) -> true)
            do! control.Begin sideshow
            nestedContinued <- nestedContinued + 1
            do! Mom.waitFor' (fun (_:Event1) -> true)
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
        let ``sideshow error while starting is propagated into the control mom``() = 

            // we start it, we control it!
        
            let sideshow = mom {
                failwith "error"
            }

            let control (control: Sideshow.Control) = mom {
                try
                    do! control.Begin sideshow
                    return false
                with e ->
                    return true
            }

            let state =
                Sideshow.attachTo control |> start

            Flux.value state |> should equal true

        [<Fact>]
        let ``sideshow error after an event is propagated to a subsequent replace``() = 

            let sideshow = mom {
                do! Mom.waitFor' (fun (_:Event1) -> true)
                failwith "error" |> ignore
            }

            let control (control: Sideshow.Control) = mom {
                do! control.Begin sideshow
                do! Mom.waitFor' (fun (_:Event1) -> true)
                try
                    do! control.Begin sideshow
                    return false
                with e ->
                    return true
            }

            let state =
                Sideshow.attachTo control |> start
                |> dispatch Event1

            Flux.value state |> should equal true


        [<Fact>]
        let ``sideshow error while cancelling is propagated in a subsequent replace``() = 

            let sideshow = mom {
                try
                    do! Mom.waitFor' (fun (_:Event2) -> true)
                finally
                    failwith "error" |> ignore
            }

            let control (control: Sideshow.Control) = mom {
                do! control.Begin sideshow
                try
                    do! control.Begin sideshow
                    return false
                with e ->
                    return true
            }

            let state =
                Sideshow.attachTo control |> start

            Flux.value state |> should equal true

        [<Fact>]
        let ``pending sideshow error overrides successful value``() = 

            let sideshow = mom {
                do! Mom.waitFor' (fun (_:Event1) -> true)
                failwith "error" |> ignore
            }

            let control (control: Sideshow.Control) = mom {
                do! control.Begin sideshow
                do! Mom.waitFor' (fun (_:Event1) -> true)
            }

            let state =
                Sideshow.attachTo control |> start
                |> dispatch Event1

            Flux.isError state |> should equal true

        [<Fact>]
        let ``pending sideshow cancellation error overrides successful value``() = 

            let sideshow = mom {
                try
                    do! Mom.waitFor' (fun (_:Event1) -> true)
                finally
                    failwith "error" |> ignore
            }

            let control (control: Sideshow.Control) = mom {
                do! control.Begin sideshow
            }

            let state =
                Sideshow.attachTo control |> start

            Flux.isError state |> should equal true

        [<Fact>]
        let ``if both sideshow and control are in error, the control error has precedence``() = 
            
            // the nested error is the control mom for the sideshow mom, so if there happens
            // and error there, it should be considered as more important.
            
            let sideshow = mom {
                try
                    do! Mom.waitFor' (fun (_:Event1) -> true)
                finally
                    failwith "error-sideshow" |> ignore
            }

            let control (control: Sideshow.Control) = mom {
                do! control.Begin sideshow
                failwith "error-control"
            }

            let state =
                Sideshow.attachTo control |> start

            (Flux.error state).Message |> should equal "error-control"

    module State = 
        [<Fact>] 
        let ``state is initially set to None``() = 
            let control (control: Sideshow.Control) = mom {
                return! control.State
            }

            let state =
                Sideshow.attachTo control |> start

            state
            |> Flux.value
            |> should equal None

        [<Fact>]
        let ``if sideshow terminates immediately, state stays at None``() = 

            let sideshow = mom {
                return ()
            }

            let control (control: Sideshow.Control) = mom {
                do! control.Begin(sideshow)
                return! control.State
            }

            let state =
                Sideshow.attachTo control |> start

            state
            |> Flux.value
            |> should equal None

        [<Fact>]
        let ``if sideshow takes time , state is set ``() = 

            let sideshow = Mom.idle

            let control (control: Sideshow.Control) = mom {
                do! control.Begin(sideshow)
                return! control.State
            }

            let state =
                Sideshow.attachTo control |> start

            state
            |> Flux.value
            |> should equal (Some ())

        [<Fact>]
        let ``sideshow state changes``() = 

            let sideshow = Mom.idle
            let sideshowNone = Mom.unit()

            let control (control: Sideshow.Control<int>) = mom {
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
            |> Flux.value
            |> should equal (Some 1, Some 2, noneInt)

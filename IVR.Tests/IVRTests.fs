namespace IVR.Tests

open NUnit.Framework
open FsUnit

open System
open System.Collections.Generic
open IVR

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
type Event3 = Event3

type Command = Command of int
    with 
    interface IVR.IReturns<string>

[<TestFixture>]
type IVRTests() =

    let dummyHost = fun _ -> null
    let start ivr = IVR.start dummyHost ivr
    let step ivr = IVR.step dummyHost ivr

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

        start a |> IVR.resultValue |> should equal 0
        ct.disposed |> should equal true


    [<Test>]
    member this.ivrIsCancelledInASequentialIVRSurroundingAWait() = 
        let ct = new CancellationTracker()

        let a = ivr {
            use x = ct
            do! IVR.waitFor' (fun (Event1) -> true)
            return 0
        }

        let started = start a
        ct.disposed |> should equal false
        step Event1 started |> ignore
        ct.disposed |> should equal true

    [<Test>]
    member this.``par: right ivr is cancelled when left throws an exception``() = 
        let ct = new CancellationTracker()

        let left = ivr {
            do! IVR.waitFor' (fun Event1 -> true)
            failwith "HellNo!"
        }

        let right = ivr {
            use ct = ct
            do! IVR.waitFor' (fun Event2 -> true)
        }

        IVR.par left right
        |> start
        |> step Event1 
        |> IVR.isError
        |> should equal true

        ct.disposed |> should equal true

    [<Test>]
    member this.``par: left ivr is cancelled when right throws an exception``() = 
        let ct = new CancellationTracker()

        let left = ivr {
            use ct = ct
            do! IVR.waitFor' (fun Event1 -> true)
        }

        let right = ivr {
            do! IVR.waitFor' (fun Event2 -> true)
            failwith "HellNo!"
        }

        IVR.par left right
        |> start
        |> step Event2
        |> IVR.isError
        |> should equal true

        ct.disposed |> should equal true

    [<Test>]
    member this.``par': right ivr is cancelled after left completes``() = 
        let ct = new CancellationTracker()

        let left = ivr {
            do! IVR.waitFor' (fun Event1 -> true)
        }

        let right = ivr {
            use ct = ct
            do! IVR.waitFor' (fun Event2 -> true)
        }

        let test = IVR.par' left right
        let started = start test
        step Event1 started |> ignore
        ct.disposed |> should equal true

    [<Test>]
    member this.``par': right ivr is cancelled after left completes and its finally handler is called``() = 
        let mutable finallyCalled = false

        let left = ivr {
            do! IVR.waitFor' (fun Event1 -> true)
        }

        let right = ivr {
            try
                do! IVR.waitFor' (fun Event2 -> true)
            finally
                finallyCalled <- true
        }

        let test = IVR.par' left right
        let started = start test
        step Event1 started |> ignore
        finallyCalled |> should equal true

    [<Test>]
    member this.``par': right ivr is cancelled after left completes and its finally ivr is run``() = 
        let mutable finallyCalled = false

        let leftResult = 1
        let rightResult = 2

        let left = ivr {
            do! IVR.waitFor' (fun Event1 -> true)
            return leftResult
        }

        let right = ivr {
            try
                do! IVR.waitFor' (fun Event2 -> true)
                return rightResult
            finally
                ivr {
                    do! IVR.waitFor' (fun Event3 -> true)
                    finallyCalled <- true
                }
        }

        let test = IVR.par' left right
        let result = 
            test
            |> start 
            |> step Event1
            |> step Event3
            
        result |> should equal (Choice<_,_>.Choice1Of2 leftResult)
        finallyCalled |> should equal true

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
        let started = start test
        step Event2 started |> ignore
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
        let started = start test
        IVR.isCompleted started |> should equal true
        ct.disposed |> should equal true

    [<Test>]
    member this.``lpar': first ivr is cancelled when the second one finishes first``() = 
        let ct = new CancellationTracker()

        let left = ivr {
            use ct = ct
            do! IVR.waitFor' (fun (Event1) -> true)
        }

        let right = ivr {
            do! IVR.waitFor' (fun (Event2) -> true)
        }

        let r = IVR.lpar' [left; right]
        let started = start r
        step Event2 started |> ignore
        ct.disposed |> should equal true

    [<Test>]
    member this.``lpar': second ivr is cancelled when the first one finishes first``() = 
        let ct = new CancellationTracker()

        let left = ivr {
            do! IVR.waitFor' (fun (Event1) -> true)
        }

        let right = ivr {
            use ct = ct
            do! IVR.waitFor' (fun (Event2) -> true)
        }

        let r = IVR.lpar' [left; right]
        let started = start r
        step Event1 started |> ignore
        ct.disposed |> should equal true

    [<Test>]
    member this.``lpar': cancellation is done in reversed order specified``() = 

        let mutable finallyTracker = []

        let a = ivr {
            try
                do! IVR.waitFor' (fun (Event1) -> true)
            finally
                finallyTracker <- finallyTracker @ ['a']
            }

        let b = ivr {
            try
                do! IVR.waitFor' (fun (Event1) -> true)
            finally
                finallyTracker <- finallyTracker @ ['b']
            }

        let c = ivr {
            try
                do! IVR.waitFor' (fun (Event2) -> true)
            finally
                finallyTracker <- finallyTracker @ ['c']
            }

        let d = ivr {
            try
                do! IVR.waitFor' (fun (Event1) -> true)
            finally
                finallyTracker <- finallyTracker @ ['d']
            }

        let e = ivr {
            try
                do! IVR.waitFor' (fun (Event1) -> true)
            finally
                finallyTracker <- finallyTracker @ ['e']
            }

        let r = IVR.lpar' [a;b;c;d;e]
        start r 
        |> step Event2
        |> ignore

        finallyTracker 
        |> should equal ['c';'e';'d';'b';'a']

    [<Test>]
    member this.``computation expression: try finally handler is run on a regular completion``() =
        let mutable x = false

        let test = ivr {
            try
                do! IVR.waitFor' (fun Event1 -> true)
            finally
                x <- true
        }

        test
        |> start
        |> step Event1
        |> ignore

        x |> should equal true

    [<Test>]
    member this.``computation expression: try finally handler is run on an error completion``() =
        let mutable x = false

        let test = ivr {
            try
                do! IVR.waitFor' (fun Event1 -> true)
                failwith "Nooooo"
            finally
                x <- true
        }

        test
        |> start
        |> step Event1
        |> ignore

        x |> should equal true

    [<Test>]
    member this.``computation expression: try finally handler is run on an error completion at startup time``() =
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

    [<Test>]
    member this.``computation expression: finally handler with ivr is called``() = 
        let mutable x = false

        let finallyIVR = ivr { x <- true }

        let test = ivr {
            try
                do! IVR.waitFor' (fun Event1 -> true)
                return 10
            finally
                finallyIVR
            }

        test
        |> start
        |> step Event1
        |> ignore

        x |> should equal true

    [<Test>]
    member this.``computation expression: finally handler with ivr is called when an exception happens``() = 
        let mutable x = false

        let finallyIVR = ivr { x <- true }

        let test = ivr {
            try
                do! IVR.waitFor' (fun Event1 -> true)
                failwith "Nooooo"
                return 10
            finally
                finallyIVR
            }

        test
        |> start
        |> step Event1
        |> ignore

        x |> should equal true

    [<Test>]
    member this.``computation expression: finally crashes``() = 
        let test = ivr {
            try
                return 10
            finally
                failwith "Nooo"
                ()
            }

        test
        |> start
        |> IVR.isError
        |> should equal true
        
    [<Test>]
    member this.``computation expression: finally ivr crashes``() = 
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
        |> IVR.isError
        |> should equal true
        
    [<Test>]
    member this.``computation expression: finally ivr crashes outside the ivr``() = 
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
        |> IVR.isError
        |> should equal true
        
    [<Test>]
    member this.``computation expression: handle exception at startup time``() =
        let mutable x = false

        let test = ivr {
            try
                failwith "Nooooo"
                return 0
            with e ->
                return 1
        }

        test
        |> start
        |> IVR.result
        |> should equal (Result 1)

    [<Test>]
    member this.``computation expression: handle exception at runtime``() =
        let mutable x = false

        let test = ivr {
            try
                do! IVR.waitFor' (fun Event1 -> true)
                failwith "Nooooo"
                return 0
            with e ->
                return 1
        }

        test
        |> start
        |> step Event1
        |> IVR.result
        |> should equal (Result 1)

    [<Test>]
    member this.``computation expression: handle exception in exception handler after wait``() =
        let mutable x = false

        let test = ivr {
            try
                do! IVR.waitFor' (fun Event1 -> true)
                failwith "Nooooo"
                return 0
            with e ->
                failwith "AGAIN"
                return 1
        }

        let res = 
            test
            |> start
            |> step Event1
        
        res |> IVR.isError |> should be True

        res 
        |> IVR.result       
        |> sprintf "%A"
        |> should contain "AGAIN"
        
    [<Test>]
    member this.``host properly cancels its ivr if it gets disposed asynchronously``() =
        let ct = new CancellationTracker()

        let ivr = ivr {
            use ct = ct
            do! IVR.waitFor' (fun (Event1) -> true)
        }

        let host _ = null
        let runtime = Runtime.newRuntime host

        async {
            try
                runtime.run ivr |> ignore
            with Cancelled ->
                ()
        } |> Async.Start

        (runtime :> IDisposable).Dispose();

        // wait a while... tbd: this makes this test brittle
        Async.Sleep(100) |> Async.RunSynchronously

        ct.disposed |> should equal true

    [<Test>]
    member this.``computation expression: yield sends a command to the host``() =
        let queue = Queue()
        
        let test = ivr {
            yield 0
        } 

        test
        |> IVR.start (fun v -> queue.Enqueue v; null)
        |> ignore

        queue |> should equal [0]


    [<Test>]
    member this.``computation expression: yield sends combined commands to the host in the right order``() =
        let queue = Queue()
        
        let test = ivr {
            yield 0
            yield 1
        } 

        test
        |> IVR.start (fun c -> queue.Enqueue c; null)
        |> ignore

        queue |> should equal [0;1]

    [<Test>]
    member this.``computation expression: after a wait, yield sends a command to the host``() =
        let queue = Queue()
        
        let test = ivr {
            yield 0
            do! IVR.waitFor' (fun Event1 -> true)
            yield 1
        } 

        test
        |> IVR.start (fun c -> queue.Enqueue c; null)
        |> IVR.step (fun c -> queue.Enqueue c; null) Event1
        |> ignore

        queue |> should equal [0;1]

    [<Test>]
    member this.``computation expression: after a wait, yield sends a command to the correct host``() =
        let queue1 = Queue()
        let queue2 = Queue()
        
        let test = ivr {
            yield 0
            do! IVR.waitFor' (fun Event1 -> true)
            yield 1
        } 

        test
        |> IVR.start (fun c -> queue1.Enqueue c; null)
        |> IVR.step (fun c -> queue2.Enqueue c; null) Event1
        |> ignore

        queue1 |> should equal [0]
        queue2 |> should equal [1]

    [<Test>]
    member this.``IVR.delay (simulated)``() = 

        let host c = 
            c |> should equal (IVR.Delay (TimeSpan(1, 0, 0)))
            1L |> box // return the 64 bit id of the Delay

        let test = IVR.delay (TimeSpan(1, 0, 0))            

        let state = 
            test |> IVR.start host 
            
        state 
        |> IVR.step host (IVR.DelayCompleted 1L)
        |> IVR.isCompleted |> should equal true

    [<Test>]
    member this.``computation expression syntax: Command with return type``() = 
        let test = ivr {
            let! r = IVR.send (Command 10)
            return r
            }

        let host (c:obj) : obj =
            match box c with
            | :? Command -> "Hello"
            | _ -> ""
            |> box

        test
        |> IVR.start host
        |> IVR.resultValue
        |> should equal "Hello"
        
    [<Test>]
    member this.``computation expression: While``() = 
        let test = ivr {
            let mutable f = 0
            while f<3 do
                do! IVR.waitFor' (fun Event1 -> true)
                f <- f + 1
            return f
        }

        let host _ = null
        test
        |> IVR.start host
        |> IVR.step host Event1
        |> IVR.step host Event1
        |> IVR.step host Event1
        |> IVR.result
        |> should equal (Result 3) 

    [<Test>]
    member this.``computation expression: For``() = 
        let test = ivr {
            for _ in [0..2] do
                do! IVR.waitFor' (fun Event1 -> true)
        }

        let host _ = null
        test
        |> IVR.start host
        |> IVR.step host Event1
        |> IVR.step host Event1
        |> IVR.step host Event1
        |> IVR.result
        |> should equal (Result ()) 
     
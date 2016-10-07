module IVR.Tests.IVRTests

open System
open System.Collections.Generic
open System.Threading
open FsUnit
open Xunit
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

type Command = 
    | Command of int
    interface IVR.ICommand<string>

let start = IVR.start
let step ev ivr = 
    match ivr with
    | Active(None, f) -> f ev
    | _ -> failwithf "step: invalid state %A" ivr
let stepH host ivr =
    match ivr with
    | Active (Some r, cont) -> 
        host r |> cont
    | _ -> failwithf "stepH: invalid state %A" ivr
        
//
// Automatic cancellation of Active ivrs.
//

[<Fact>]
let disposeIsCalledInASequentialIVR() = 
    let ct = new CancellationTracker()

    let a = ivr {
        use x = ct
        return 0
    }

    start a |> IVR.resultValue |> should equal 0
    ct.disposed |> should equal true

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
            do! IVR.waitFor' (fun (Event1) -> true)
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
    |> step Event1
    |> ignore

    trace |> should equal [1;2;0]

[<Fact>]
let ivrIsCancelledInASequentialIVRSurroundingAWait() = 
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

[<Fact>]
let ``par: right ivr is cancelled when left throws an exception``() = 
    let ct = new CancellationTracker()

    let left = ivr {
        do! IVR.waitFor' (fun Event1 -> true)
        failwith "HellNo!"
    }

    let right = ivr {
        use ct = ct
        do! IVR.waitFor' (fun Event2 -> true)
    }

    IVR.join left right
    |> start
    |> step Event1 
    |> IVR.isError
    |> should equal true

    ct.disposed |> should equal true

[<Fact>]
let ``par: left ivr is cancelled when right throws an exception``() = 
    let ct = new CancellationTracker()

    let left = ivr {
        use ct = ct
        do! IVR.waitFor' (fun Event1 -> true)
    }

    let right = ivr {
        do! IVR.waitFor' (fun Event2 -> true)
        failwith "HellNo!"
    }

    IVR.join left right
    |> start
    |> step Event2
    |> IVR.isError
    |> should equal true

    ct.disposed |> should equal true

[<Fact>]
let ``par': right ivr is cancelled after left completes``() = 
    let ct = new CancellationTracker()

    let left = ivr {
        do! IVR.waitFor' (fun Event1 -> true)
    }

    let right = ivr {
        use ct = ct
        do! IVR.waitFor' (fun Event2 -> true)
    }

    let test = IVR.first left right
    let started = start test
    step Event1 started |> ignore
    ct.disposed |> should equal true

[<Fact>]
let ``par': right ivr is cancelled after left completes and its finally handler is called``() = 
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

    let test = IVR.first left right
    let started = start test
    step Event1 started |> ignore
    finallyCalled |> should equal true

[<Fact>]
let ``par': right ivr is cancelled after left completes and its finally ivr is run``() = 
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

    let test = IVR.first left right
    let result = 
        test
        |> start 
        |> step Event1
        |> step Event3
            
    result 
    |> IVR.resultValue 
    |> should equal (Choice<int,int>.Choice1Of2 leftResult)
    finallyCalled |> should equal true

[<Fact>]
let ``par': left ivr is cancelled after right completes``() = 
    let ct = new CancellationTracker()

    let left = ivr {
        use ct = ct
        do! IVR.waitFor' (fun (Event1) -> true)
    }

    let right = ivr {
        do! IVR.waitFor' (fun (Event2) -> true)
    }

    let test = IVR.first left right
    let started = start test
    step Event2 started |> ignore
    ct.disposed |> should equal true


[<Fact>]
let ``par': left ivr is cancelled when the right one is completed after startup``() = 
    let ct = new CancellationTracker()

    let left = ivr {
        use ct = ct
        do! IVR.waitFor' (fun (Event1) -> true)
    }

    let right = ivr {
        return 0
    }

    let test = IVR.first left right
    let started = start test
    IVR.isCompleted started |> should equal true
    ct.disposed |> should equal true

[<Fact>]
let ``any: first ivr is cancelled when the second one finishes first``() = 
    let ct = new CancellationTracker()

    let left = ivr {
        use ct = ct
        do! IVR.waitFor' (fun (Event1) -> true)
    }

    let right = ivr {
        do! IVR.waitFor' (fun (Event2) -> true)
    }

    let r = IVR.any [left; right]
    let started = start r
    step Event2 started |> ignore
    ct.disposed |> should equal true

[<Fact>]
let ``any: second ivr is cancelled when the first one finishes first``() = 
    let ct = new CancellationTracker()

    let left = ivr {
        do! IVR.waitFor' (fun (Event1) -> true)
    }

    let right = ivr {
        use ct = ct
        do! IVR.waitFor' (fun (Event2) -> true)
    }

    let r = IVR.any [left; right]
    let started = start r
    step Event1 started |> ignore
    ct.disposed |> should equal true

[<Fact>]
let ``any: cancellation is done in reversed order specified``() = 

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

    let r = IVR.any [a;b;c;d;e]
    start r 
    |> step Event2
    |> ignore

    finallyTracker 
    |> should equal ['c';'e';'d';'b';'a']

[<Fact>]
let ``computation expression: try finally handler is run on a regular completion``() =
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

[<Fact>]
let ``computation expression: try finally handler is run on an error completion``() =
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

[<Fact>]
let ``computation expression: finally handler with ivr is called when an exception happens``() = 
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
    |> IVR.isError
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
    |> IVR.isError
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
    |> IVR.isError
    |> should equal true
        
[<Fact>]
let ``computation expression: handle exception at startup time``() =
    let mutable x = false

    let test = ivr {
        try
            failwith "Nooooo"
            return 0
        with _ ->
            return 1
    }

    test
    |> start
    |> IVR.result
    |> should equal (Value 1)

[<Fact>]
let ``computation expression: handle exception at runtime``() =
    let mutable x = false

    let test = ivr {
        try
            do! IVR.waitFor' (fun Event1 -> true)
            failwith "Nooooo"
            return 0
        with _ ->
            return 1
    }

    test
    |> start
    |> step Event1
    |> IVR.result
    |> should equal (Value 1)

[<Fact>]
let ``computation expression: handle exception in exception handler after wait``() =
    let mutable x = false

    let test = ivr {
        try
            do! IVR.waitFor' (fun Event1 -> true)
            failwith "Nooooo"
            return 0
        with _ ->
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
    |> should haveSubstring "AGAIN"
        
[<Fact>]
let ``host properly cancels its ivr if it gets disposed asynchronously``() =
    let ct = new CancellationTracker()

    let ivr = ivr {
        use ct = ct
        do! IVR.waitFor' (fun (Event1) -> true)
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
let ``when an ivr gets cancelled, it ends cancelled but does not throw any exception``() = 

    let ivr = ivr {
        try
            do! IVR.waitFor' (fun (Event1) -> true)
        with e ->
            failwith "fail"
    }

    let res = 
        ivr
        |> start
        |> step IVR.TryCancel

    IVR.isCancelled res |> should be True

[<Fact>]
let ``when an ivr gets cancelled, it runs the finally part``() = 

    let mutable runFinally = false

    let ivr = ivr {
        try
            do! IVR.waitFor' (fun (Event1) -> true)
        finally
            runFinally <- true
    }

    let res = 
        ivr
        |> start
        |> step IVR.TryCancel
    
    runFinally |> should be True
    IVR.isCancelled res |> should be True

[<Fact>]
let ``when an ivr gets cancelled in a while loop, it does not continue outside of it``() = 

    let mutable continued = false

    let ivr = ivr {
        let mutable f = 0
        while true do
            do! IVR.waitFor' (fun (Event1) -> true)
        continued <- true
    }

    let res = 
        ivr
        |> start
        |> step IVR.TryCancel
    
    IVR.isCancelled res |> should be True

[<Fact(Skip="See issue #4")>]
let ``when one part of a parallel ivr gets cancelled, the followup ivr continues while the cancellation is run``() = 

    let primaryIVR = ivr {
            do! IVR.waitFor' (fun (Event1) -> true)
        }

    // cancellation of the secondary IVR takes forvever
    let secondaryIVR = ivr {
        try
            do! IVR.waitFor' (fun (Event2) -> true)
        finally
            IVR.idle
    }

    let test = ivr {
            do! IVR.any [primaryIVR; secondaryIVR]
            do! IVR.waitFor' (fun (Event3) -> true)
        }

    let res = 
        test
        |> start
        |> step Event1
        |> step Event3

    res |> IVR.isCompleted |> should be True        
        
[<Fact>]
let ``computation expression: IVR.post sends a command to the host``() =
    let queue = Queue()
        
    let test = ivr {
        do! IVR.post 0
    } 

    test
    |> start
    |> stepH (fun v -> queue.Enqueue v; null)
    |> ignore

    queue |> Seq.toList |> should equal [box 0]


[<Fact>]
let ``computation expression: IVR.post sends combined commands to the host in the right order``() =
    let queue = Queue()
        
    let test = ivr {
        do! IVR.post 0
        do! IVR.post 1
    } 

    test
    |> start
    |> stepH queue.Enqueue
    |> stepH queue.Enqueue
    |> ignore

    queue |> Seq.toList |> should equal [box 0; box 1]

[<Fact>]
let ``computation expression: after a wait, IVR.post sends a command to the host``() =
    let queue = Queue()
        
    let test = ivr {
        do! IVR.post 0
        do! IVR.waitFor' (fun Event1 -> true)
        do! IVR.post 1
    } 

    test
    |> start
    |> stepH queue.Enqueue
    |> step Event1
    |> stepH queue.Enqueue
    |> ignore

    queue |> Seq.toList |> should equal [box 0;box 1]

[<Fact>]
let ``IVR.delay (simulated)``() = 

    let host (c: obj) = 
        c |> should equal (IVR.Delay (TimeSpan(1, 0, 0)))
        Id 1L |> box // return the 64 bit id of the Delay

    let test = IVR.delay (TimeSpan(1, 0, 0))            

    let state = 
        test |> start
            
    state 
    |> stepH host
    |> step (IVR.DelayCompleted (Id 1L))
    |> IVR.isCompleted |> should equal true

[<Fact>]
let ``IVR.delay completes immediately in case of a zero delay``() = 

    let test = IVR.delay TimeSpan.Zero            

    let state = 
        test |> start
            
    state 
    |> IVR.isCompleted |> should be True


[<Fact>]
let ``computation expression syntax: Command with return type``() = 
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
    |> start
    |> stepH host
    |> IVR.resultValue
    |> should equal "Hello"

[<Fact>]
let ``computation expression syntax: Command with return type (implicit send!)``() = 
    let test = ivr {
        let! r = Command 10
        return r
    }

    let host (c:obj) : obj =
        match box c with
        | :? Command -> "Hello"
        | _ -> ""
        |> box

    test
    |> start
    |> stepH host
    |> IVR.resultValue
    |> should equal "Hello"
        
[<Fact>]
let ``computation expression: While``() = 
    let test = ivr {
        let mutable f = 0
        while f<3 do
            do! IVR.waitFor' (fun Event1 -> true)
            f <- f + 1
        return f
    }

    test
    |> start
    |> step Event1
    |> step Event1
    |> step Event1
    |> IVR.result
    |> should equal (Value 3) 

[<Fact>]
let ``computation expression: For``() = 
    let test = ivr {
        for _ in [0..2] do
            do! IVR.waitFor' (fun Event1 -> true)
    }

    test
    |> start
    |> step Event1
    |> step Event1
    |> step Event1
    |> IVR.result
    |> should equal (Value ()) 
     
namespace IVR

open System

(*
    An IVR is a definition of an asynchronous process with the following properties:
    
    - can be paused and ended at any time.
    - can synchronously wait and respond to events.
    - can be combined in parallel or sequentially.
*)

type Event = obj

type Result<'result> =
    | Result of 'result
    | Cancelled
    with 
        member this.map f =
            match this with
            | Cancelled -> Cancelled
            | Result r -> Result (f r)

type AIVR<'result> = 
    | Active of (Event -> AIVR<'result>)
    | Completed of Result<'result>

type IVR<'result> = 
    | Delay of (unit -> AIVR<'result>)

type 'result result = Result<'result>
type 'result aivr = AIVR<'result>
type 'result ivr = IVR<'result>

module TimeSpanExtensions =

    type Int32 with
        member this.seconds = TimeSpan.FromSeconds(float this)
        member this.milliseconds = TimeSpan.FromMilliseconds(float this)

    type Double with
        member this.seconds = TimeSpan.FromSeconds(this)
        member this.milliseconds = TimeSpan.FromMilliseconds(this)
        

module IVR = 

    // 
    // IVR Primitives
    //

    /// Start up this ivr.

    let start (Delay f) = f()

    /// Continue an active ivr with one event.
    let step ivr e = 
        match ivr with
        // may be we should start it here, too?
        //> no: delays are not supported, once an IVR starts, subsequential
        //> IVRs do have to be started before stepping through

        | Completed _ -> failwith "IVR.step: ivr is completed"
        | Active f -> f e

    /// Continue an active or completed ivr with one event. If the ivr is completed, the ivr is the result.
    let private progress ivr e = 
        match ivr with 
        | Completed _ -> ivr
        | Active f -> f e 

    /// Returns true if the ivr is completed (i.e. has a result).
    let isCompleted ivr = 
        match ivr with
        | Completed _ -> true
        | _ -> false

    let isCancelled ivr = 
        match ivr with
        | Completed Cancelled -> true
        | Completed _ -> false
        | _ -> failwithf "IVR.isCancelled incomplete: %A" ivr

    /// Returns the result of a completed ivr.
    let result ivr = 
        match ivr with
        | Completed r -> r
        | _ -> failwith "IVR.result: ivr is not completed"

    /// Returns the resulting value of a completed ivr.
    let resultValue ivr = 
        match ivr with
        | Completed (Result r) -> r
        | Completed _ -> failwithf "IVR.resultValue ivr has not been completed with a resulting value: %A" ivr
        | _ -> failwithf "IVR.resultValue: ivr is not completed: %A" ivr

    /// The event that is sent to an active IVR when it gets cancelled. The only accepted return state is Cancelled.
    type Cancel = Cancel

    /// Cancels the ivr. For now it is expected that the ivr immediately returns the Cancelled state.
    let cancel ivr = 
        match ivr with
        | Active f -> 
            let r = f Cancel 
            match r with
            | Completed Cancelled -> ()
            | _ -> failwithf "IVR.cancel: cancellation unhandled: %A" r
        | _ -> failwith "IVR.cancel: ivr is not active"

    //
    // IVR Combinators
    //

    /// Runs two ivrs in parallel, the resulting ivr completes, when both ivrs are completed.
    /// events are delivered first to ivr1, then to ivr2.

    /// Note that par retains the result of the completed ivr, which
    /// could lead to leaks in nested parallel ivrs of which the result
    /// is never processed.

    let par (ivr1 : IVR<'r1>) (ivr2 : IVR<'r2>) : IVR<'r1 * 'r2> =

        let rec active ivr1 ivr2 e = 
            next (progress ivr1 e) (progress ivr2 e)

        and next ivr1 ivr2 =
            match ivr1, ivr2 with
            | Completed r1, Completed r2 -> 
                match r1, r2 with
                | Result r1, Result r2 -> (r1, r2) |> Result |> Completed
                | _ -> Cancelled |> Completed
            | _ -> active ivr1 ivr2 |> Active

        fun () -> next (start ivr1) (start ivr2)
        |> Delay

    /// Combine a list of ivrs so that they run in parallel

    let lpar (ivrs: 'r ivr list) : 'r list ivr = 

        let rec active ivrs e =
            let ivrs = 
                ivrs
                |> List.map (fun ivr -> progress ivr e)
            next ivrs

        and next ivrs = 
            let anyActive = 
                ivrs
                |> List.exists (isCompleted >> not)
            match anyActive with
            | true -> active ivrs |> Active
            | false ->
                let anyCancelled = 
                    ivrs
                    |> List.exists (isCancelled)

                if anyCancelled then
                    Completed Cancelled
                else
                    ivrs
                    |> List.map resultValue
                    |> Result
                    |> Completed

        fun () -> next (List.map start ivrs)
        |> Delay

    // specialized version of lpar that removes results from processing when
    // the return type is unit.
    // tbd
    // lpar_ 

    /// Runs two ivrs in parallel, the resulting ivr completes with the result of the one that finishes first.
    /// events are delivered to ivr1 and then to ivr2, so ivr1 has an advantage when both complete in response to
    /// the same event. Note that if ivr1 completes, no event is delivered to ivr2.
    
    let par' (ivr1 : IVR<'r1>) (ivr2 : IVR<'r2>) : IVR<Choice<'r1, 'r2>> =

        let rec loop ivr1 ivr2 e = 

            match ivr1, ivr2 with
            | Active f1, Active f2 -> 
                let ivr1 = f1 e
                match ivr1 with
                | Completed r1 -> 
                    cancel ivr2
                    r1.map Choice1Of2 |> Completed
                | _ ->
                let ivr2 = f2 e
                match ivr2 with
                | Completed r2 -> 
                    cancel ivr1
                    r2.map Choice2Of2 |> Completed
                | _ -> Active <| loop ivr1 ivr2
            | _ -> failwithf "IVR.par': unexpected %A, %A" ivr1 ivr2

        fun () -> 
            let ivr1 = start ivr1
            match ivr1 with
            | Completed r1 -> r1.map Choice1Of2 |> Completed
            | _ ->
            let ivr2 = start ivr2
            match ivr2 with
            | Completed r2 -> r2.map Choice2Of2 |> Completed
            | _ ->
            Active <| loop ivr1 ivr2
            
        |> Delay

    /// Runs a list of ivrs in parallel and finish with the first one that completes.

    let lpar' (ivrs: 'r ivr list) : 'r ivr =

        let folder f (ivrs, result) ivr = 
            // don't step pending ivrs, if we already have a result!
            // also: this is a kind of idle part of this algorithm, actually
            // we need to break the folder as soon we see a Completed ivr
            match result with
            | Some r -> [], Some r
            | None ->
            let ivr = f ivr
            match ivr with
            | Active _ -> (ivr::ivrs), None
            | Completed r -> [], Some r

        let stepFolder e = folder (fun ivr -> step ivr e)
        let startFolder = folder start

        let rec active ivrs e = 

            ivrs 
            |> List.fold (stepFolder e) ([], None)
            |> next
           
        and next (ivrs, result) = 
            match result with
            | Some r -> Completed r
            | None -> Active (active (ivrs |> List.rev))

        fun () ->
            ivrs
            |> List.fold startFolder ([], None)
            |> next

        |> Delay

    //
    // more basic primitives
    //

    /// An IVR that waits for some event given a function that returns (Some result) or None.

    let wait f =
        let rec waiter e =  
            match box e with
            | :? Cancel -> Cancelled |> Completed
            | _ ->
            match f e with
            | Some r -> r |> Result |> Completed
            | None -> Active waiter

        fun () ->
            Active waiter
        |> Delay

    /// Waits for some event with a predicate that returns
    /// true or false

    let wait' predicate = 
        let f e = 
            let r = predicate e
            match r with 
            | true -> Some ()
            | false -> None

        wait f

    /// Waits for an event of a type derived by a function passed in.

    let waitFor (f : 'e -> 'r option) = 

        let f (e : Event) = 
            match e with
            | :? 'e as e -> f e
            | _ -> None

        wait f

    let waitFor' (f : 'e -> bool) =
        let f (e: Event) =
            match e with
            | :? 'e as e -> f e
            | _ -> false

        wait' f

    //
    // Simple computation expression to build sequential IVR processes
    //

    type IVRBuilder<'result>() = 
        member this.Bind(ivr: 'r ivr, cont: 'r -> 'r2 aivr) : 'r2 aivr = 

            let rec next ivr = 
                match ivr with
                | Active f -> Active (f >> next)
                | Completed (Result r) -> (cont r)
                | Completed Cancelled -> Completed Cancelled

            next (start ivr)

        member this.Return(v) = v |> Result |> Completed

        member this.ReturnFrom ivr = start ivr

        // We want to delay the startup of an IVR to the moment IVR.start is run, because
        // computation expressions may contain regular code at the beginning that would run at
        // instantiation of the expression and not when we start / run the ivr
        member this.Delay (f : unit -> 'r aivr) : 'r ivr = Delay f

        // zero makes only sense for IVR<unit>
        member this.Zero () = () |> Result |> Completed

        member this.Using(disposable : 't, body : 't -> 'u aivr when 't :> IDisposable) : 'u aivr = 
            
            let rec next ivr =
                match ivr with
                | Active f -> Active (f >> next)
                | Completed _ ->
                    disposable.Dispose()
                    ivr

            disposable
            |> body //< need a try finally around the body call here to enable exception handling!
            |> next

    let ivr<'result> = IVRBuilder<'result>()

    /// Maps the ivr's result type.

    let map f ivr' = 
        ivr {
            let! r = ivr'
            return f r
        }

    /// Ignores the ivr's result type.

    let ignore ivr = ivr |> map ignore

    //
    // TPL naming scheme
    //

    /// Wait for all ivrs to finish.
    let waitAll ivrs = lpar ivrs

    /// Wait for any one of the ivrs to finish.
    let waitAny ivrs = lpar' ivrs 

module BuilderExtensions = 
    let ivr<'r> = IVR.ivr<'r>

// a little inception may not do any harm :)

[<assembly:AutoOpen("IVR.TimeSpanExtensions")>]
[<assembly:AutoOpen("IVR.BuilderExtensions")>]
do ()

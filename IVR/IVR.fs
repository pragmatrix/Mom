namespace IVR

open System

(*
    An IVR is a definition of an asynchronous process with the following properties:
    
    - can be ended at any given time without a concrete reason.
    - can only synchronously respond to events. 
        - a response is either a notification, a Result or an Error

    When an IVR is active, it gets every event in the system.
*)


type ExitEvent = ExitEvent

type Event = obj
type Notification = obj

type IVR<'result> = 
    | Delay of (unit -> IVR<'result>)
    | Active of (Event -> IVR<'result>)
    | Completed of 'result

module IVR = 

    // start up this ivr 
    // this goes through all Delay states and stops at Active or Completed.

    let rec start ivr = 
        match ivr with
        | Delay f -> f() |> start
        | _ -> ivr

    // run two ivrs in parallel, the resulting ivr completes, when both ivrs are completed.
    // events are delivered first to ivr1, then to ivr2.

    let par (ivr1 : IVR<'r1>) (ivr2 : IVR<'r2>) : IVR<'r1 * 'r2> =

        let rec active ivr1 ivr2 e = 

            let ivr1, ivr2 = 
                match ivr1, ivr2 with
                | Active f1, Active f2 -> f1 e, f2 e
                | Completed _, Active f2 -> ivr1, f2 e
                | Active f1, Completed _ -> f1 e, ivr2
                | _ -> failwithf "par: unexpected %A, %A" ivr1 ivr2

            next ivr1 ivr2

        and next ivr1 ivr2 =

            match ivr1, ivr2 with
            | Completed r1, Completed r2 -> Completed (r1, r2)
            | _ -> active ivr1 ivr2 |> Active

        fun () -> next (start ivr1) (start ivr2)
        |> Delay

    // run two ivrs in parallel, the resulting ivr completes with the result of the one that finishes first.
    // events are delivered to ivr1 and then to ivr2, so ivr1 has an advantage when both complete in response to
    // the same event. Note that if ivr1 completes, no event is delivered to ivr2.
    
    let par' (ivr1 : IVR<'r1>) (ivr2 : IVR<'r2>) : IVR<Choice<'r1, 'r2>> =

        let rec loop ivr1 ivr2 e = 

            match ivr1, ivr2 with
            | Active f1, Active f2 -> 
                let ivr1 = f1 2
                match ivr1 with

                Active <| loop (f1 e) (f2 e)
            | Completed r1, Active _ ->
                Completed <| Choice1Of2 r1
            | Active _, Completed r2 ->
                Completed <| Choice2Of2 r2
            | _ -> failwithf "par': unexpected %A, %A" ivr1 ivr2

        fun () -> 
            let ivr1 = start ivr1
            match ivr1 with
            | Completed r1 -> Completed <| Choice1Of2 r1
            | _ ->
            let ivr2 = start ivr2
            match ivr2 with
            | Completed r2 -> Completed <| Choice2Of2 r2
            | _ ->
            Active <| loop (start ivr1) (start ivr2)

        |> Delay

(*
    Simple computation expression, to build sequential IVR processes by
    continuations / monads.
*)

type IVRBuilder<'result>() = 
    member this.Bind(ivr: IVR<'r>, cont: 'r -> IVR<'r2>) : IVR<'r2> = 

        let rec loop (ivr:IVR<'r>) e =
            match ivr with
            | Delay _ -> failwith "bind: unexpected Delay"
            | Active f -> 
                let i = f e
                Active (loop i)
            | Completed r -> cont r
            
        Active (loop ivr)

    member this.Return(v) = Completed v

    // We want to delay the startup of an IVR to the moment IVR.start is run.
    member this.Delay (f : unit -> IVR<'r>) : IVR<'r> = Delay f

[<AutoOpen>]
module Helper = 
    let ivr<'result> = IVRBuilder<'result>()







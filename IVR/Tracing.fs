namespace IVR

module Tracing = 

    type private Tracer(instance) = 
        member this.traceEvent e =
            ()
        member this.traceResult r =
            ()

    let private beginTrace instance = Tracer(instance)

    /// For an IVR to be eligable for tracing, it must be declared. The instance given is to 
    /// associate the IVR, it can be a string or any other instance, like a union cases for example.
    #if false
    let declare instance ivr = 

        fun () ->
            let tracer = beginTrace instance

            let rec next ivr = 
                match ivr with
                | Active _ -> Active (fun e -> tracer.traceEvent e; (ivr |> IVR.step e |> next))
                | Completed r ->
                    tracer.traceResult r
                    ivr

            ivr |> IVR.start |> next
        |> Delay


    #endif

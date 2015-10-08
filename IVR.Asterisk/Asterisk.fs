namespace IVR.Asterisk

open IVR

open AsterNET.ARI.Models
open AsterNET.ARI.Actions

module Asterisk =
    
    type IAsteriskCommand<'r> =
        inherit IDispatch<IAsteriskActions>
        inherit IVR.IReturns<'r>

    type IAsteriskCommand =
        inherit IDispatch<IAsteriskActions>

    type GetInfo = GetInfo of only: string option with
        interface IAsteriskCommand<AsteriskInfo> with
            member this.dispatch asterisk = 
                let (GetInfo only) = this
                asterisk.GetInfo(opts only)
                |> box

    type GetGlobalVar = GetGlobalVar of variable: string with
        interface IAsteriskCommand<Variable> with
            member this.dispatch asterisk = 
                let (GetGlobalVar variable) = this
                asterisk.GetGlobalVar(variable)
                |> box

    type SetGlobalVar = SetGlobalVar of variable: string * value: string option with
        interface IAsteriskCommand with
            member this.dispatch asterisk = 
                let (SetGlobalVar(variable, value)) = this
                asterisk.SetGlobalVar(variable, opts value)
                |> box


    [<AbstractClass; Sealed>]
    type Asterisk() =
        static member GetInfo(?only) = 
            GetInfo only

        static member GetGlobalVar(variable) = 
            GetGlobalVar variable

        static member SetGlobalVar(variable, ?value) = 
            SetGlobalVar(variable, value)

    
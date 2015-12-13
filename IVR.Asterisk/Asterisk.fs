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

    type GetObject = {
        configClass: string
        objectType: string
        id: string
        } with
        interface IAsteriskCommand<(string * string) list> with
            member this.dispatch asterisk = 
                asterisk.GetObject(this.configClass, this.objectType, this.id)
                |> Seq.toList
                |> List.map(fun ct -> ct.Attribute, ct.Value)
                |> box

    type UpdateObject = {
        configClass: string
        objectType: string
        id: string
        fields: (string * string) list option
        } with
        interface IAsteriskCommand<(string * string) list> with
            member this.dispatch asterisk = 
                asterisk.UpdateObject(this.configClass, this.objectType, this.id, optvars this.fields)
                |> Seq.toList
                |> List.map(fun ct -> ct.Attribute, ct.Value)
                |> box

    type DeleteObject = {
        configClass: string
        objectType: string
        id: string
        } with
        interface IAsteriskCommand with
            member this.dispatch asterisk = 
                asterisk.DeleteObject(this.configClass, this.objectType, this.id)
                |> box

    type GetInfo = GetInfo of only: string option with
        interface IAsteriskCommand<AsteriskInfo> with
            member this.dispatch asterisk = 
                let (GetInfo only) = this
                asterisk.GetInfo(opts only)
                |> box

    type ListModules = ListModules with
        interface IAsteriskCommand<Module list> with
            member this.dispatch asterisk = 
                asterisk.ListModules()
                |> Seq.toList
                |> box

    type GetModule = GetModule of moduleName: string with
        interface IAsteriskCommand<Module> with
            member this.dispatch asterisk = 
                let (GetModule moduleName) = this
                asterisk.GetModule(moduleName)
                |> box

    type LoadModule = LoadModule of moduleName: string with
        interface IAsteriskCommand with
            member this.dispatch asterisk = 
                let (LoadModule moduleName) = this
                asterisk.LoadModule(moduleName)
                |> box

    type UnloadModule = UnloadModule of moduleName: string with
        interface IAsteriskCommand with
            member this.dispatch asterisk = 
                let (UnloadModule moduleName) = this
                asterisk.UnloadModule(moduleName)
                |> box

    type ReloadModule = ReloadModule of moduleName: string with
        interface IAsteriskCommand with
            member this.dispatch asterisk = 
                let (ReloadModule moduleName) = this
                asterisk.ReloadModule(moduleName)
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
        static member GetObject(configClass, objectType, id) = 
            { GetObject.configClass = configClass; objectType = objectType; id = id }
        static member UpdateObject(configClass, objectType, id, ?fields) = 
            { UpdateObject.configClass = configClass; objectType = objectType; id = id; fields = fields }
        static member DeleteObject(configClass, objectType, id) = 
            { DeleteObject.configClass = configClass; objectType = objectType; id = id }
        
        static member GetInfo(?only) = 
            GetInfo only
        
        static member ListModules() = 
            ListModules
        static member GetModule(moduleName) = 
            GetModule(moduleName)
        static member LoadModule(moduleName) = 
            LoadModule(moduleName)
        static member UnloadModule(moduleName) = 
            UnloadModule(moduleName)
        static member ReloadMoudle(moduleName) = 
            ReloadModule(moduleName)

        static member GetGlobalVar(variable) = 
            GetGlobalVar variable
        static member SetGlobalVar(variable, ?value) = 
            SetGlobalVar(variable, value)

    
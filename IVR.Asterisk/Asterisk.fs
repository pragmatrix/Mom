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
        static member getObject(configClass, objectType, id) = 
            { GetObject.configClass = configClass; objectType = objectType; id = id }
            |> IVR.send
        static member updateObject(configClass, objectType, id, ?fields) = 
            { UpdateObject.configClass = configClass; objectType = objectType; id = id; fields = fields }
            |> IVR.send

        static member deleteObject(configClass, objectType, id) = 
            { DeleteObject.configClass = configClass; objectType = objectType; id = id }
            |> IVR.post

        static member getInfo(?only) = 
            GetInfo only
            |> IVR.send
        
        static member listModules() = 
            ListModules |> IVR.send
        static member getModule(moduleName) = 
            GetModule(moduleName) |> IVR.send
        static member loadModule(moduleName) = 
            LoadModule(moduleName) |> IVR.post
        static member unloadModule(moduleName) = 
            UnloadModule(moduleName) |> IVR.post
        static member reloadMoudle(moduleName) = 
            ReloadModule(moduleName) |> IVR.post

        static member getGlobalVar(variable) = 
            GetGlobalVar variable
            |> IVR.send
        static member setGlobalVar(variable, ?value) = 
            SetGlobalVar(variable, value)
            |> IVR.post

    
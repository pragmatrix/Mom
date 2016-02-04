namespace IVR.Asterisk

open IVR

open AsterNET.ARI.Models
open AsterNET.ARI.Actions

module Asterisk =
    
    type GetObject = {
        configClass: string
        objectType: string
        id: string
        } with
        interface IDispatchAction<(string * string) list> with
            member this.dispatch client = 
                client.Asterisk.GetObject(this.configClass, this.objectType, this.id)
                |> Seq.toList
                |> List.map(fun ct -> ct.Attribute, ct.Value)
                |> box

    type UpdateObject = {
        configClass: string
        objectType: string
        id: string
        fields: (string * string) list option
        } with
        interface IDispatchAction<(string * string) list> with
            member this.dispatch client = 
                client.Asterisk.UpdateObject(this.configClass, this.objectType, this.id, optvars this.fields)
                |> Seq.toList
                |> List.map(fun ct -> ct.Attribute, ct.Value)
                |> box

    type DeleteObject = {
        configClass: string
        objectType: string
        id: string
        } with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                client.Asterisk.DeleteObject(this.configClass, this.objectType, this.id)
                |> box

    type GetInfo = GetInfo of only: string option with
        interface IDispatchAction<AsteriskInfo> with
            member this.dispatch client = 
                let (GetInfo only) = this
                client.Asterisk.GetInfo(opts only)
                |> box

    type ListModules = ListModules with
        interface IDispatchAction<Module list> with
            member this.dispatch client = 
                client.Asterisk.ListModules()
                |> Seq.toList
                |> box

    type GetModule = GetModule of moduleName: string with
        interface IDispatchAction<Module> with
            member this.dispatch client = 
                let (GetModule moduleName) = this
                client.Asterisk.GetModule(moduleName)
                |> box

    type LoadModule = LoadModule of moduleName: string with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (LoadModule moduleName) = this
                client.Asterisk.LoadModule(moduleName)
                |> box

    type UnloadModule = UnloadModule of moduleName: string with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (UnloadModule moduleName) = this
                client.Asterisk.UnloadModule(moduleName)
                |> box

    type ReloadModule = ReloadModule of moduleName: string with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (ReloadModule moduleName) = this
                client.Asterisk.ReloadModule(moduleName)
                |> box

    type GetGlobalVar = GetGlobalVar of variable: string with
        interface IDispatchAction<Variable> with
            member this.dispatch client = 
                let (GetGlobalVar variable) = this
                client.Asterisk.GetGlobalVar(variable)
                |> box

    type SetGlobalVar = SetGlobalVar of variable: string * value: string option with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (SetGlobalVar(variable, value)) = this
                client.Asterisk.SetGlobalVar(variable, opts value)
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

    
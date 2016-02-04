namespace IVR.Asterisk

open IVR

open AsterNET.ARI.Actions

module DeviceStates =

    type DeviceState = 
        | NotInUse = 0
        | InUse = 1
        | Busy = 2
        | Invalid = 3
        | Unavailable = 4
        | Ringing = 5
        | RingInUse = 6
        | OnHold = 7

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DeviceState = 
        let tos = 
            function
            | DeviceState.NotInUse -> "NOT_INUSE"
            | DeviceState.InUse -> "INUSE"
            | DeviceState.Busy -> "BUSY"
            | DeviceState.Invalid -> "INVALID"
            | DeviceState.Unavailable -> "UNAVAILABLE"
            | DeviceState.Ringing -> "RINGING"
            | DeviceState.RingInUse -> "RINGINUSE"
            | DeviceState.OnHold -> "ONHOLD"
            | ds -> failwithf "invalid device state: %A" ds

    type List = List with
        interface IDispatchAction<DeviceState list> with
            member this.dispatch client =
                client.DeviceStates.List() |> Seq.toList
                |> box

    type Get = Get of deviceName: string with
        interface IDispatchAction<DeviceState> with
            member this.dispatch client = 
                let (Get deviceName) = this
                client.DeviceStates.Get(deviceName)
                |> box

    type Update = Update of deviceName: string * deviceState: DeviceState with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (Update (deviceName, deviceState)) = this
                client.DeviceStates.Update(deviceName, deviceState |> DeviceState.tos)
                |> box

    type Delete = Delete of deviceName: string with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (Delete deviceName) = this
                client.DeviceStates.Delete(deviceName)
                |> box
    [<AbstractClass;Sealed>]
    type DeviceStates() =
        static member list() = 
            List
            |> IVR.send
        static member get(deviceName) =
            Get(deviceName)
            |> IVR.send
        static member update(deviceName, deviceState) = 
            Update(deviceName, deviceState)
            |> IVR.post
        static member delete(deviceName) = 
            Delete(deviceName)
            |> IVR.post
        
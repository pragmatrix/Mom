namespace IVR.Asterisk

open IVR

open AsterNET.ARI.Actions

module DeviceStates =

    type IDeviceStatesCommand<'r> =
        inherit IDispatch<IDeviceStatesActions>
        inherit IVR.IReturns<'r>

    type IDeviceStatesCommand =
        inherit IDispatch<IDeviceStatesActions>

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
        interface IDeviceStatesCommand<DeviceState list> with
            member this.dispatch deviceStates =
                deviceStates.List() |> Seq.toList
                |> box

    type Get = Get of deviceName: string with
        interface IDeviceStatesCommand<DeviceState> with
            member this.dispatch deviceStates = 
                let (Get deviceName) = this
                deviceStates.Get(deviceName)
                |> box

    type Update = Update of deviceName: string * deviceState: DeviceState with
        interface IDeviceStatesCommand with
            member this.dispatch deviceStates = 
                let (Update (deviceName, deviceState)) = this
                deviceStates.Update(deviceName, deviceState |> DeviceState.tos)
                |> box

    type Delete = Delete of deviceName: string with
        interface IDeviceStatesCommand with
            member this.dispatch deviceStates = 
                let (Delete deviceName) = this
                deviceStates.Delete(deviceName)
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
        
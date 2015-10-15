namespace IVR.Asterisk

open IVR

open AsterNET.ARI.Models
open AsterNET.ARI.Actions

module Recordings =

    type IRecordingsCommand<'r> =
        inherit IDispatch<IRecordingsActions>
        inherit IVR.IReturns<'r>

    type IRecordingsCommand =
        inherit IDispatch<IRecordingsActions>

    type ListStored = ListStored with
        interface IRecordingsCommand<StoredRecording list> with
            member this.dispatch recordings = 
                recordings.ListStored() |> Seq.toList 
                |> box

    type GetStored = GetStored of recordingName: string with
        interface IRecordingsCommand<StoredRecording> with
            member this.dispatch recordings = 
                let (GetStored recordingName) = this
                recordings.GetStored(recordingName)
                |> box

    type DeleteStored = DeleteStored of recordingName: string with
        interface IRecordingsCommand with
            member this.dispatch recordings = 
                let (DeleteStored recordingName) = this
                recordings.DeleteStored(recordingName)
                |> box

    type CopyStored = CopyStored of recordingname: string * destinationRecordingName: string with
        interface IRecordingsCommand<StoredRecording> with
            member this.dispatch recordings = 
                let (CopyStored (recordingName, destinationRecordingName)) = this
                recordings.CopyStored(recordingName, destinationRecordingName)
                |> box

    type GetLive = GetLive of recordingName: string with
        interface IRecordingsCommand<LiveRecording> with
            member this.dispatch recordings = 
                let (GetLive recordingName) = this
                recordings.GetLive(recordingName)
                |> box

    type Cancel = Cancel of recordingName: string with
        interface IRecordingsCommand with
            member this.dispatch recordings = 
                let (Cancel recordingName) = this
                recordings.Cancel(recordingName)
                |> box

    type Stop = Stop of recordingName: string with
        interface IRecordingsCommand with
            member this.dispatch recordings = 
                let (Stop recordingName) = this
                recordings.Stop(recordingName)
                |> box

    type Pause = Pause of recordingName: string with
        interface IRecordingsCommand with
            member this.dispatch recordings = 
                let (Pause recordingName) = this
                recordings.Pause(recordingName)
                |> box
    
    type Unpause = Unpause of recordingName: string with
        interface IRecordingsCommand with
            member this.dispatch recordings = 
                let (Unpause recordingName) = this
                recordings.Unpause(recordingName)
                |> box

    type Mute = Mute of recordingName: string with
        interface IRecordingsCommand with
            member this.dispatch recordings = 
                let (Mute recordingName) = this
                recordings.Mute(recordingName)
                |> box

    type Unmute = Unmute of recordingName: string with
        interface IRecordingsCommand with
            member this.dispatch recordings = 
                let (Unmute recordingName) = this
                recordings.Unmute(recordingName)
                |> box
    
    [<AbstractClass;Sealed>]
    type Recordings() =
        static member ListStored() =
            ListStored
        static member GetStored(recordingName) = 
            GetStored(recordingName)
        static member DeleteStored(recordingName) = 
            DeleteStored(recordingName)
        static member CopyStored(recordingName, destinationRecordingName) = 
            CopyStored(recordingName, destinationRecordingName)
        static member GetLive(recordingName) =
            GetLive(recordingName)
        static member Cancel(recordingName) = 
            Cancel(recordingName)
        static member Stop(recordingName) = 
            Stop(recordingName)
        static member Pause(recordingName) = 
            Pause(recordingName)
        static member Unpause(recordingName) =
            Unpause(recordingName)
        static member Mute(recordingName) = 
            Mute(recordingName)
        static member Unmute(recordingName) = 
            Unmute(recordingName)



    
    




    
                
    
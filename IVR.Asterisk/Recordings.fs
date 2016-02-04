namespace IVR.Asterisk

open IVR

open AsterNET.ARI.Models

module Recordings =

    type ListStored = ListStored with
        interface IDispatchAction<StoredRecording list> with
            member this.dispatch client = 
                client.Recordings.ListStored() |> Seq.toList 
                |> box

    type GetStored = GetStored of recordingName: string with
        interface IDispatchAction<StoredRecording> with
            member this.dispatch client = 
                let (GetStored recordingName) = this
                client.Recordings.GetStored(recordingName)
                |> box

    type DeleteStored = DeleteStored of recordingName: string with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (DeleteStored recordingName) = this
                client.Recordings.DeleteStored(recordingName)
                |> box

    type CopyStored = CopyStored of recordingname: string * destinationRecordingName: string with
        interface IDispatchAction<StoredRecording> with
            member this.dispatch client = 
                let (CopyStored (recordingName, destinationRecordingName)) = this
                client.Recordings.CopyStored(recordingName, destinationRecordingName)
                |> box

    type GetLive = GetLive of recordingName: string with
        interface IDispatchAction<LiveRecording> with
            member this.dispatch client = 
                let (GetLive recordingName) = this
                client.Recordings.GetLive(recordingName)
                |> box

    type Cancel = Cancel of recordingName: string with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (Cancel recordingName) = this
                client.Recordings.Cancel(recordingName)
                |> box

    type Stop = Stop of recordingName: string with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (Stop recordingName) = this
                client.Recordings.Stop(recordingName)
                |> box

    type Pause = Pause of recordingName: string with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (Pause recordingName) = this
                client.Recordings.Pause(recordingName)
                |> box
    
    type Unpause = Unpause of recordingName: string with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (Unpause recordingName) = this
                client.Recordings.Unpause(recordingName)
                |> box

    type Mute = Mute of recordingName: string with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (Mute recordingName) = this
                client.Recordings.Mute(recordingName)
                |> box

    type Unmute = Unmute of recordingName: string with
        interface IDispatchAction<unit> with
            member this.dispatch client = 
                let (Unmute recordingName) = this
                client.Recordings.Unmute(recordingName)
                |> box
    
    [<AbstractClass;Sealed>]
    type Recordings() =
        static member listStored() =
            ListStored
            |> IVR.send
        static member getStored(recordingName) = 
            GetStored(recordingName)
            |> IVR.send
        static member deleteStored(recordingName) = 
            DeleteStored(recordingName)
            |> IVR.post
        static member copyStored(recordingName, destinationRecordingName) = 
            CopyStored(recordingName, destinationRecordingName)
            |> IVR.send
        static member getLive(recordingName) =
            GetLive(recordingName)
            |> IVR.send
        static member cancel(recordingName) = 
            Cancel(recordingName)
            |> IVR.post
        static member stop(recordingName) = 
            Stop(recordingName)
            |> IVR.post
        static member pause(recordingName) = 
            Pause(recordingName)
            |> IVR.post
        static member unpause(recordingName) =
            Unpause(recordingName)
            |> IVR.post
        static member mute(recordingName) = 
            Mute(recordingName)
            |> IVR.post
        static member unmute(recordingName) = 
            Unmute(recordingName)
            |> IVR.post



    
    




    
                
    
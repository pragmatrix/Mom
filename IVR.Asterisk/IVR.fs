namespace IVR

open System
open AsterNET.ARI
open AsterNET.ARI.Models

module IVR =
    let waitForStasisStart() = 
        IVR.waitFor (fun (e: StasisStartEvent) -> Some e)

    let waitForPlaybackFinished(playbackId: string) = 
        IVR.waitFor' (fun (e: PlaybackFinishedEvent) -> e.Playback.Id = playbackId)
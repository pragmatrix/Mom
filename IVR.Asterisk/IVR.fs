namespace IVR.Asterisk

open IVR

open System
open System.Linq
open AsterNET.ARI
open AsterNET.ARI.Models
open System.Collections.Generic

module IVR =
    let waitForStasisStart() = 
        IVR.waitFor (fun (e: StasisStartEvent) -> Some e)

    let waitForPlaybackFinished(playbackId: string) = 
        IVR.waitFor' (fun (e: PlaybackFinishedEvent) -> e.Playback.Id = playbackId)

    type IAriActionClientDispatch =
        abstract member dispatch : IAriActionClient -> Response

    type IDispatchAction<'r> = 
        inherit IAriActionClientDispatch
        inherit IVR.IReturns<'r>

    let inline optref (r: 'a option) = 
        match r with
        | Some r -> r
        | None -> null

    let inline opts (o: string option) = optref o

    let inline opt (i: 'i option) = 
        match i with
        | Some i -> Nullable(i)
        | None -> Nullable()

    let inline optvars (i: ('a * 'b) list option ) = 
        i
        |> Option.map (fun l -> l.ToDictionary(fst, snd)) 
        |> optref

    type TimeSpan with
        static member toms(ts: TimeSpan) = 
            ts.TotalMilliseconds |> int
        static member tosec(ts: TimeSpan) =
            ts.TotalSeconds |> int

    /// Comma separated strings
    let inline css (strings: string list) =
        String.Join(",", strings |> List.toArray)

    //
    // Shared
    //

    type IfExists =
        | Fail = 0
        | Overwrite = 1
        | Append = 2

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module IfExists =
        let tos = 
            function 
            | IfExists.Fail -> "fail"
            | IfExists.Overwrite -> "overwrite"
            | IfExists.Append -> "append"
            | v -> failwithf "invalid IfExists: %A" v

    type TerminateOn = 
        | None = 0
        | Any = 1
        | Star = 2
        | Hash = 3

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TerminateOn =
        let tos = 
            function
            | TerminateOn.None -> "none"
            | TerminateOn.Any -> "any"
            | TerminateOn.Star -> "*"
            | TerminateOn.Hash -> "#"
            | v -> failwithf "invalid TerminateOn: %A" v
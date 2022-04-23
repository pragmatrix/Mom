namespace Mom

open System.Threading

[<Struct>]
type Id = 
    | Id of int64
    member this.Value = let (Id v) = this in v

/// This implements process local ids that are serializable.
[<RequireQualifiedAccess>]
module Ids =
    
    [<Struct>]
    type Generator = {
        Id: int64 ref
    } with 
        member this.GenerateId() = 
            Interlocked.Increment(this.Id)
            |> Id

    let newGenerator() = 
        { Id = ref 0L }

namespace IVR

open System.Threading

type Id = int64

/// This implements process local ids that are serializable.
[<RequireQualifiedAccess>]
module Ids =
    
    type Generator = {
        Id: Id ref
    } with 
        member this.GenerateId() = 
            Interlocked.Increment(this.Id)

    let newGenerator() = 
        { Id = ref 0L }

namespace IVR

open System.Threading

type Id = int64

/// This implements process local ids that are serializable.
[<RequireQualifiedAccess>]
module Ids =
    
    type Generator = {
        id: Id ref
    }
    with 
        member this.generateId() = 
            Interlocked.Increment(this.id)

    let newGenerator() = 
        { id = ref 0L }

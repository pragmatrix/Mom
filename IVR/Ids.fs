namespace IVR

open System.Threading

type Id = int64

(*
    The module Ids implements process local ids, that are serializable.
*)

module Ids =
    
    type Generator = {
        id: Id ref
    }
    with 
        member this.generateId() = 
            Interlocked.Increment(this.id)

    let newGenerator() = 
        { id = ref 0L }

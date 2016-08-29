module IVR.Tests.SystemTests

open FsUnit
open Xunit

[<Fact>]
let boxedValueOfUnitIsNull() = 
    () |> box 
    |> should equal null



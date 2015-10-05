namespace IVR.Tests

open NUnit.Framework
open FsUnit

[<TestFixture>]
type SystemTests() = 

    [<Test>]
    member this.boxedValueOfUnitIsNull() = 
        () |> box 
        |> should equal null



module IVR.Tests.SystemTests

open FsUnit
open Xunit
open Microsoft.FSharp.Quotations

[<Fact>]
let boxedValueOfUnitIsNull() = 
    () |> box 
    |> should equal null


let someGenericFunction<'t>() = ()

[<Fact>]
let canGetMethodInfoFromGenericModuleFunctionViaQuotations() = 
        
    let exp = <@@ someGenericFunction<obj>() @@>
    let mi = 
        match exp with
        | Patterns.Call(_, mi, _) -> mi
        | _ -> failwith "internal error"

    mi.Name |> should equal "someGenericFunction"
    mi.IsGenericMethod |> should equal true
    mi.IsGenericMethodDefinition |> should equal false
    mi.GetGenericMethodDefinition().IsGenericMethodDefinition |> should equal true

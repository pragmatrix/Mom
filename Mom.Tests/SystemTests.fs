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

type SomeInterface = interface end

let someGenericFunction2<'t>(i: SomeInterface) : unit -> Async<obj> = 
    let f() = async {
        return box null
    }
    f
    

[<Fact>]
let canGetMethodInfoFromGenericModuleFunctionWithParameterViaQuotations() = 
        
    let exp = <@@ someGenericFunction2<obj>((box null) :?> SomeInterface) @@>
    let mi = 
        match exp with
        | Patterns.Call(_, mi, _) -> mi
        | _ -> failwith "internal error"

    mi.Name |> should equal "someGenericFunction2"
    mi.IsGenericMethod |> should equal true
    mi.IsGenericMethodDefinition |> should equal false
    mi.GetGenericMethodDefinition().IsGenericMethodDefinition |> should equal true

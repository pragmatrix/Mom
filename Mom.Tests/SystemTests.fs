module Mom.Tests.SystemTests

open FsUnit
open Xunit
open Microsoft.FSharp.Quotations
open System.Runtime.ExceptionServices

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
        | _ -> failwith "Internal error"

    mi.Name |> should equal "someGenericFunction"
    mi.IsGenericMethod |> should equal true
    mi.IsGenericMethodDefinition |> should equal false
    mi.GetGenericMethodDefinition().IsGenericMethodDefinition |> should equal true

type SomeInterface = interface end

let someGenericFunction2<'t>(_i: SomeInterface) : unit -> Async<obj> = 
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
        | _ -> failwith "Internal error"

    mi.Name |> should equal "someGenericFunction2"
    mi.IsGenericMethod |> should equal true
    mi.IsGenericMethodDefinition |> should equal false
    mi.GetGenericMethodDefinition().IsGenericMethodDefinition |> should equal true

let private functionThatThrows() = 
    failwith "error here"

[<Fact(Skip="fails on CI")>]
let ``ExceptionDispatchInfo properly preserves stack traces``() = 
    
    let dispatchInfo =
        try
            functionThatThrows()
        with e ->
            ExceptionDispatchInfo.Capture(e)

    try
        dispatchInfo.Throw()
        failwith "unexpected"
    with e ->
        let str = string e
        printfn $"{str}"
        str |> should haveSubstring "functionThatThrows"

    
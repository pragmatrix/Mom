/// A service that runs synchronous and asynchronous functions that are directly 
/// attached to the request by providing a Execute() function.
///
/// The service associates the async functions to the request type, so that if multiple requests
/// are scheduled, their async functions are guaranteed to never run at the same time. 
///
/// To implement that, it installs a mailbox processor for each type. 
/// If the async functions need to run in parallel, use another type.

namespace Mom

open System
open System.Collections.Generic
open System.Runtime.ExceptionServices
open Microsoft.FSharp.Quotations

/// This is the non-generic marker interface in terms of the response type, so that the service can
/// unambiguously detect if it's called for.
type IInlineAsyncRequest<'context> = interface end

type IInlineRequest<'context> = interface end

type IInlineAsyncRequest<'context, 'response> =
    inherit IInlineAsyncRequest<'context>
    inherit Mom.IAsyncRequest<'response>
    
    /// Implement this member to specify the async function that
    /// should be run
    abstract member Execute : 'context -> Async<'response>

type IInlineRequest<'context, 'response> =
    inherit IInlineRequest<'context>
    inherit Mom.IRequest<'response>

    /// Implement this member to specify the function that should be run
    abstract member Execute : 'context -> 'response

[<AutoOpen>]
module private Processor =

    [<NoComparison; NoEquality>] 
    type Reply = 
        | Ok of obj
        | Error of ExceptionDispatchInfo

    [<NoComparison; NoEquality>] 
    type Message =
        | RunAsync of (unit -> Async<obj>) * AsyncReplyChannel<Reply>

    type Processor = MailboxProcessor<Message>

    let createProcessor() : Processor = 
        let processor = MailboxProcessor.Start <| fun _ -> async.Return()

        let rec loop() = async {
            let! (RunAsync(job, reply)) = processor.Receive()
            try
                let! r = job()
                reply.Reply <| Ok r
            with e ->
                reply.Reply <| Error (ExceptionDispatchInfo.Capture e)
            return! loop()
        }

        Async.Start <| loop()
        processor

// We need some way to box the Execute member's response type.
module private ExecuteWrapperAsync =

    let inlineAsyncRequestGenericTypeDefinition = 
        typeof<IInlineAsyncRequest<obj, obj>>.GetGenericTypeDefinition()

    // The generic function to resolve the async function.
    let wrap<'context, 'response> (request: IInlineAsyncRequest<'context>) : 'context -> Async<obj> =
        // can't return this as a fun / lambda, somehow these screws up the method
        // extractor below.
        let f (context: 'context) =
            async {
                let request = request :?> IInlineAsyncRequest<'context, 'response>
                let! r = request.Execute(context)
                return box r
            }
        f

    // Use Quotations to get out the `MethodInfo` of the `wrap` function.
    let wrapMethodInfo : Reflection.MethodInfo = 
        let exp = <@@ wrap<obj, obj>((box null) :?> IInlineAsyncRequest<obj>) @@>
        match exp with
        | Patterns.Call(_, mi, _) -> mi.GetGenericMethodDefinition()
        | _ -> failwith "Internal error"

    // The generic function to create a typed `AsyncResponse`.
    let createResponse<'response> (id: Id, result: obj) : obj =
        let unboxedResult : obj Flux.result = unbox result
        let r : Mom.AsyncResponse<'response> = Mom.AsyncResponse(id, unboxedResult |> Flux.Result.map unbox)
        box r

    let responseMethodInfo = 
        let exp = <@@ createResponse<obj>(Id 0L, null) @@>
        match exp with
        | Patterns.Call(_, mi, _) -> mi.GetGenericMethodDefinition()
        | _ -> failwith "Internal error"

    let resolveResponseType (t: Type) = 
        let genericInterface = 
            t.GetInterfaces()
            |> Seq.find(fun i -> i.IsGenericType && i.GetGenericTypeDefinition() = inlineAsyncRequestGenericTypeDefinition)

        genericInterface.GetGenericArguments().[0]

    type ExecuteF<'context> = IInlineAsyncRequest<'context> -> 'context -> Async<obj>

    let createBoxedExecute (responseType: Type) : ExecuteF<'context> =
        let m = wrapMethodInfo.MakeGenericMethod(typeof<'context>, responseType)
        let f = 
            Delegate.CreateDelegate(typeof<Func<IInlineAsyncRequest<'context>, 'context -> Async<obj>>>, m)
            :?> Func<IInlineAsyncRequest<'context>, 'context -> Async<obj>>
        f.Invoke

    type ResponseF = Id * obj -> obj

    let createBoxedResponse (responseType: Type) : ResponseF = 
        let m = responseMethodInfo.MakeGenericMethod responseType
        let f = 
            Delegate.CreateDelegate(typeof<Func<Id, obj, obj>>, m)
            :?> Func<Id, obj, obj>
        f.Invoke

module private ExecuteWrapper =

    let inlineRequestGenericTypeDefinition = 
        typeof<IInlineRequest<obj, obj>>.GetGenericTypeDefinition()

    let wrap<'context, 'response> (request: IInlineRequest<'context>) : 'context -> obj =
        // can't return this as a fun / lambda, somehow these screws up the method
        // extractor below.
        let f (context: 'context) =
            let request = request :?> IInlineRequest<'context, 'response>
            request.Execute(context)
            |> box
        f

    // use Quotations to get out the MethodInfo of the wrap function.
    let wrapMethodInfo<'context> = 
        let exp = <@@ wrap<'context, obj>((box null) :?> IInlineRequest<'context>) @@>
        match exp with
        | Patterns.Call(_, mi, _) -> mi.GetGenericMethodDefinition()
        | _ -> failwith "internal error"

    let resolveResponseType (t: Type) : Type = 
        let genericInterface = 
            t.GetInterfaces()
            |> Seq.find(fun i -> i.IsGenericType && i.GetGenericTypeDefinition() = inlineRequestGenericTypeDefinition)
        genericInterface.GetGenericArguments().[1]

    type ExecuteF<'context> = IInlineRequest<'context> -> 'context -> obj

    let createBoxedExecute (responseType: Type) : ExecuteF<'context> =
        let m = wrapMethodInfo.MakeGenericMethod(typeof<'context>, responseType)
        let f = 
            Delegate.CreateDelegate(typeof<Func<IInlineRequest<'context>, 'context -> obj>>, m)
            :?> Func<IInlineRequest<'context>, 'context -> obj>
        f.Invoke

module InlineRequestService =

    /// Create a service that is able to support IInlineRequest<'context, 'response> and IInlineAsyncService<'context, 'response>
    let create (context: 'context) : Runtime.IServiceContext -> Flux.Request -> Flux.Response option = 

        fun (service : Runtime.IServiceContext) ->

        let helperTableAsync = Dictionary<Type, ExecuteWrapperAsync.ExecuteF<'context> * Processor * ExecuteWrapperAsync.ResponseF>()
        let resolveHelperAsync interfaceType =
            match helperTableAsync.TryGetValue interfaceType with
            | true, helper -> helper
            | _ ->
            let responseType = ExecuteWrapperAsync.resolveResponseType interfaceType
            let helper = ExecuteWrapperAsync.createBoxedExecute responseType, createProcessor(), ExecuteWrapperAsync.createBoxedResponse responseType
            helperTableAsync.Add(interfaceType, helper)
            helper

        let executeTable = Dictionary<Type, ExecuteWrapper.ExecuteF<'context>>()
        let resolveExecute interfaceType =
            match executeTable.TryGetValue interfaceType with
            | true, helper -> helper
            | _ ->
            let responseType = ExecuteWrapper.resolveResponseType interfaceType
            let executeF = ExecuteWrapper.createBoxedExecute responseType
            executeTable.Add(interfaceType, executeF)
            executeF

        function
        | :? IInlineAsyncRequest<'context> as r ->
            let execute, processor, createResponse = resolveHelperAsync (r.GetType())
            let f : 'context -> Async<obj> = execute r
            
            let id = Mom.generateAsyncRequestId()

            Async.Start <| async {
                let! reply = 
                    processor.PostAndAsyncReply 
                    <| fun replyChannel -> RunAsync((fun () -> f context), replyChannel)
                
                let result =
                    match reply with
                    | Ok response -> Flux.Value response
                    | Error e -> Flux.Error e

                createResponse(id, result)
                |> service.ScheduleEvent
            }

            Some (box id)

        | :? IInlineRequest<'context> as r ->
            let execute = resolveExecute (r.GetType())
            Some(execute r context)

        | _ -> None

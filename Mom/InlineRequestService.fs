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

/// This is the non-generic marker interface, so that the service can 
/// unambiguously detect if it's called for.
type IInlineAsyncRequest = interface end

type IInlineRequest = interface end

type IInlineAsyncRequest<'response> =
    inherit IInlineAsyncRequest
    inherit Mom.IAsyncRequest<'response>
    
    /// Implement this member to specify the async function that
    /// should be run
    abstract member Execute : unit -> Async<'response>

type IInlineRequest<'response> =
    inherit IInlineRequest
    inherit Mom.IRequest<'response>

    /// Implement this member to specify the function that should be run
    abstract member Execute : unit -> 'response

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
        typeof<IInlineAsyncRequest<obj>>.GetGenericTypeDefinition()

    // the generic function to resolve the async function.

    let wrap<'response> (request: IInlineAsyncRequest) : unit -> Async<obj> =
        // can't return this as a fun / lambda, somehow these screws up the method
        // extractor below.
        let f() =
            async {
                let request = request :?> IInlineAsyncRequest<'response>
                let! r = request.Execute()
                return box r
            }
        f

    // use Quotations to get out the MethodInfo of the wrap function.
    let wrapMethodInfo = 
        let exp = <@@ wrap<obj>((box null) :?> IInlineAsyncRequest) @@>
        match exp with
        | Patterns.Call(_, mi, _) -> mi.GetGenericMethodDefinition()
        | _ -> failwith "internal error"

    // the generic function to create a typed AsyncResponse.
    let createResponse<'response> (id: Id, result: obj) : obj =
        let unboxedResult : obj Flux.result = unbox result
        let r : Mom.AsyncResponse<'response> = Mom.AsyncResponse(id, unboxedResult |> Flux.Result.map unbox)
        box r

    let responseMethodInfo = 
        let exp = <@@ createResponse<obj>(Id 0L, null) @@>
        match exp with
        | Patterns.Call(_, mi, _) -> mi.GetGenericMethodDefinition()
        | _ -> failwith "internal error"

    let resolveResponseType (t: Type) = 
        let genericInterface = 
            t.GetInterfaces()
            |> Seq.find(fun i -> i.IsGenericType && i.GetGenericTypeDefinition() = inlineAsyncRequestGenericTypeDefinition)

        genericInterface.GetGenericArguments().[0]

    type ExecuteF = IInlineAsyncRequest -> unit -> Async<obj>

    let createBoxedExecute (responseType: Type) : ExecuteF =
        let m = wrapMethodInfo.MakeGenericMethod responseType
        let f = 
            Delegate.CreateDelegate(typeof<Func<IInlineAsyncRequest, unit -> Async<obj>>>, m)
            :?> Func<IInlineAsyncRequest, unit -> Async<obj>>
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
        typeof<IInlineRequest<obj>>.GetGenericTypeDefinition()

    let wrap<'response> (request: IInlineRequest) : unit -> obj =
        // can't return this as a fun / lambda, somehow these screws up the method
        // extractor below.
        let f() =
            let request = request :?> IInlineRequest<'response>
            request.Execute()
            |> box
        f

    // use Quotations to get out the MethodInfo of the wrap function.
    let wrapMethodInfo = 
        let exp = <@@ wrap<obj>((box null) :?> IInlineRequest) @@>
        match exp with
        | Patterns.Call(_, mi, _) -> mi.GetGenericMethodDefinition()
        | _ -> failwith "internal error"

    let resolveResponseType (t: Type) = 
        let genericInterface = 
            t.GetInterfaces()
            |> Seq.find(fun i -> i.IsGenericType && i.GetGenericTypeDefinition() = inlineRequestGenericTypeDefinition)
        genericInterface.GetGenericArguments().[0]

    type ExecuteF = IInlineRequest -> unit -> obj

    let createBoxedExecute (responseType: Type) : ExecuteF =
        let m = wrapMethodInfo.MakeGenericMethod responseType
        let f = 
            Delegate.CreateDelegate(typeof<Func<IInlineRequest, unit -> obj>>, m)
            :?> Func<IInlineRequest, unit -> obj>
        f.Invoke

module InlineRequestService =

    /// Create a service that is able to support IInlineRequest<'response> and IInlineAsyncService<'response>
    let create() : Runtime.IServiceContext -> Flux.Request -> Flux.Response option = 

        fun (service : Runtime.IServiceContext) ->

        let helperTableAsync = Dictionary<Type, ExecuteWrapperAsync.ExecuteF * Processor * ExecuteWrapperAsync.ResponseF>()
        let resolveHelperAsync interfaceType =
            match helperTableAsync.TryGetValue interfaceType with
            | true, helper -> helper
            | _ ->
            let responseType = ExecuteWrapperAsync.resolveResponseType interfaceType
            let helper = ExecuteWrapperAsync.createBoxedExecute responseType, createProcessor(), ExecuteWrapperAsync.createBoxedResponse responseType
            helperTableAsync.Add(interfaceType, helper)
            helper

        let executeTable = Dictionary<Type, ExecuteWrapper.ExecuteF>()
        let resolveExecute interfaceType =
            match executeTable.TryGetValue interfaceType with
            | true, helper -> helper
            | _ ->
            let responseType = ExecuteWrapper.resolveResponseType interfaceType
            let executeF = ExecuteWrapper.createBoxedExecute responseType
            executeTable.Add(interfaceType, executeF)
            executeF

        function
        | :? IInlineAsyncRequest as r ->
            let execute, processor, createResponse = resolveHelperAsync (r.GetType())
            let f = execute r
            
            let id = Mom.generateAsyncRequestId()

            Async.Start <| async {
                let! reply = 
                    processor.PostAndAsyncReply 
                    <| fun replyChannel -> RunAsync(f, replyChannel)
                
                let result =
                    match reply with
                    | Ok response -> Flux.Value response
                    | Error e -> Flux.Error e

                createResponse(id, result)
                |> service.ScheduleEvent
            }

            Some (box id)

        | :? IInlineRequest as r ->
            let execute = resolveExecute (r.GetType())
            Some(execute r ())

        | _ -> None

/// A service that runs async functions that are directly attached to the
/// request.
/// The service binds the async functions to the request type, so that if multiple requests
/// are scheduled, their async functions are guaranteed to never  run at the same time. 
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

type IInlineAsyncRequest<'response> =
    inherit IInlineAsyncRequest
    inherit Mom.IAsyncRequest<'response>
    
    /// Implement this member to specify the async function that
    /// should be run
    abstract member Execute : unit -> Async<'response>

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
[<AutoOpen>]
module private ExecuteWrapper =

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

    // the generic function to create a typed AsyncResponse.
    let createResponse<'response> (id: Id, result: obj) : obj =
        let unboxedResult : Mom.result<obj> = unbox result
        let r : Mom.AsyncResponse<'response> = Mom.AsyncResponse(id, unboxedResult |> Mom.Result.map unbox)
        box r

    // use Quotations to get out the MethodInfo of the wrap function.
    let wrapMethodInfo = 
        let exp = <@@ wrap<obj>((box null) :?> IInlineAsyncRequest) @@>
        match exp with
        | Patterns.Call(_, mi, _) -> mi.GetGenericMethodDefinition()
        | _ -> failwith "internal error"

    let responseMethodInfo = 
        let exp = <@@ createResponse<obj>(Id 0L, null) @@>
        match exp with
        | Patterns.Call(_, mi, _) -> mi.GetGenericMethodDefinition()
        | _ -> failwith "internal error"

    type ExecuteF = IInlineAsyncRequest -> unit -> Async<obj>

    let resolveResponseType (t: Type) = 
        let genericInterface = 
            t.GetInterfaces()
            |> Seq.find(fun i -> i.IsGenericType && i.GetGenericTypeDefinition() = inlineAsyncRequestGenericTypeDefinition)

        genericInterface.GetGenericArguments().[0]

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

module InlineAsyncRequestService =

    /// Create a service that is able to support IInlineAsyncService<'response>
    let create() : Runtime.IServiceContext -> Flux.Request -> Flux.Response option = 

        fun (service : Runtime.IServiceContext) ->

        let helperTable = Dictionary<Type, ExecuteF * Processor * ResponseF>()
        let resolveHelper t =
            match helperTable.TryGetValue t with
            | true, helper -> helper
            | _ ->
            let responseType = resolveResponseType t
            let helper = createBoxedExecute responseType, createProcessor(), createBoxedResponse responseType
            helperTable.Add(t, helper)
            helper

        function
        | :? IInlineAsyncRequest as r ->
            let execute, processor, createResponse = resolveHelper (r.GetType())
            let f = execute r
            
            let id = Mom.generateAsyncRequestId()

            Async.Start <| async {
                let! reply = 
                    processor.PostAndAsyncReply 
                    <| fun replyChannel -> RunAsync(f, replyChannel)
                
                let result =
                    match reply with
                    | Ok response -> Mom.Value response
                    | Error e -> Mom.Error e.SourceException

                createResponse(id, result)
                |> service.ScheduleEvent
            }

            Some (box id)

        | _ -> None
        
namespace Step9.Infrastructure

type EventSource = System.Guid

type EventProducer<'Event> =
  'Event list -> 'Event list


type EventEnvelope<'Event> =
  {
    Source : EventSource
    Event : 'Event
  }

type EventListener<'Event> =
  EventEnvelope<'Event> -> unit


type EventStore<'Event> =
  {
    Get : unit -> EventEnvelope<'Event> list
    GetStream : EventSource -> EventEnvelope<'Event> list
    Append : EventEnvelope<'Event> list -> unit
    Subscribe : EventListener<'Event>-> unit
    OnError : IEvent<exn>
  }


type InfrastructureError =
  | Error of string
  | Exception of exn


type EventResult<'Event> =
  Result<EventEnvelope<'Event> list, InfrastructureError>

type EventStorage<'Event> =
  {
    Get : unit -> EventResult<'Event>
    GetStream : EventSource -> EventResult<'Event>
    Append : EventEnvelope<'Event> list -> unit
    OnError : IEvent<exn>
  }

type Projection<'State,'Event> =
  {
    Init : 'State
    Update : 'State -> 'Event -> 'State
  }

type QueryResult<'Result> =
  | Handled of obj
  | NotHandled

type  QueryHandler<'Query,'Result> =
  {
    Handle : 'Query -> QueryResult<'Result>
  }

type ReadModel<'Event, 'Query, 'Result> =
  {
    EventListener : EventListener<'Event>
    QueryHandler : QueryHandler<'Query,'Result>
    OnError : IEvent<exn>
  }


type CommandHandler<'Command> =
  {
    Handle : EventSource -> 'Command -> unit
    OnError : IEvent<exn>
  }

type Behaviour<'Command,'Event> =
  'Command -> EventProducer<'Event>


/// A wrapper for MailboxProcessor that catches all unhandled exceptions
/// and reports them via the 'OnError' event. Otherwise, the API
/// is the same as the API of 'MailboxProcessor'
type Agent<'T>(f:Agent<'T> -> Async<unit>) as self =
  // Create an event for reporting errors
  let errorEvent = Event<_>()
  // Start standard MailboxProcessor
  let inbox = new MailboxProcessor<'T>(fun _ ->
    async {
      // Run the user-provided function & handle exceptions
      try return! f self
      with e -> errorEvent.Trigger(e)
    })

  /// Triggered when an unhandled exception occurs
  member __.OnError = errorEvent.Publish
  /// Starts the mailbox processor
  member __.Start() = inbox.Start()
  /// Receive a message from the mailbox processor
  member __.Receive() = inbox.Receive()
  /// Post a message to the mailbox processor
  member __.Post(value:'T) = inbox.Post value

  member __.PostAndReply(f: AsyncReplyChannel<'a> -> 'T) = inbox.PostAndReply f

  /// Start the mailbox processor
  static member Start f =
    let agent = new Agent<_>(f)
    agent.Start()
    agent


type EventSourced<'Comand,'Event,'Query,'Result>
  (eventStoreInit : EventStorage<'Event> -> EventStore<'Event>,
   eventStorageInit : unit -> EventStorage<'Event>,
   commandHandlerInit : EventStore<'Event> -> CommandHandler<'Comand>,
   queryHandlerInit : unit -> QueryHandler<'Query,'Result> * (QueryHandler<'Query,'Result> -> unit),
   readmodelsInit : (unit -> ReadModel<'Event, 'Query, 'Result>) list) =

  let eventStorage = eventStorageInit()

  let eventStore = eventStoreInit eventStorage

  let commandHandler = commandHandlerInit eventStore

  let queryHandler,addQueryHandler = queryHandlerInit()

  do
    eventStore.OnError.Add(printfn "eventStore Error: %A")
    eventStorage.OnError.Add(printfn "eventStorage Error: %A")
    commandHandler.OnError.Add(printfn "commandHandler Error: %A")

    readmodelsInit
    |> List.iter (fun readmodel ->
        let readmodel = readmodel()
        eventStore.OnError.Add(printfn "EventStore Error: %A")
        do readmodel.EventListener |> eventStore.Subscribe
        do readmodel.QueryHandler |> addQueryHandler)

  member __.HandleCommand eventSource command =
    commandHandler.Handle eventSource command

  member __.HandleQuery query =
    queryHandler.Handle query

  member __.GetAllEvents () =
    eventStore.Get()

  member __.GetStream eventSource =
    eventStore.GetStream eventSource


module EventStorage =
  type Msg<'Event> =
    private
    | Get of AsyncReplyChannel<EventResult<'Event>>
    | GetStream of EventSource * AsyncReplyChannel<EventResult<'Event>>
    | Append of EventEnvelope<'Event> list


  module InMemory =

    let private streamFor source history =
      history

    let initialize () : EventStorage<'Event> =
      let history : EventEnvelope<'Event> list = []

      let agent =
        Agent<Msg<_>>.Start(fun inbox ->
          let rec loop history =
            async {
              let! msg = inbox.Receive()

              match msg with
              | Get reply ->
                  history
                  |> Ok
                  |> reply.Reply

                  return! loop history

              | GetStream (source,reply) ->
                  history
                  |> streamFor source
                  |> Ok
                  |> reply.Reply

                  return! loop history

              | Append events ->
                  return! loop (history @ events)
            }

          loop history
        )

      {
        Get = fun () ->  agent.PostAndReply Get
        GetStream = fun eventSource -> agent.PostAndReply (fun reply -> (eventSource,reply) |> GetStream)
        Append = Append >> agent.Post
        OnError = agent.OnError
      }

  module FileStorage =
    open System.IO
    open Thoth.Json.Net

    let private writeEvents store events =
      use streamWriter = new StreamWriter(store, true)
      events
      |> List.map (fun eventEnvelope -> Encode.Auto.toString(0,eventEnvelope))
      |> List.iter streamWriter.WriteLine

      do streamWriter.Flush()


    let initialize store : EventStorage<'Event> =
      let agent =
        Agent<Msg<_>>.Start(fun inbox ->
          let rec loop () =
            async {
              let! msg = inbox.Receive()

              match msg with
              | Get reply ->
                  File.ReadLines(store)
                  |> Seq.traverseResult Decode.Auto.fromString<EventEnvelope<'Event>>
                  |> fun x -> x
                  |> Result.mapError Error
                  |> Result.map List.ofSeq
                  |> reply.Reply

                  return! loop()

              | GetStream (source,reply) ->
                  File.ReadLines(store)
                  |> Seq.traverseResult Decode.Auto.fromString<EventEnvelope<'Event>>
                  |> Result.mapError Error
                  |> Result.map (Seq.filter (fun ee -> ee.Source = source) >> List.ofSeq)
                  |> reply.Reply

                  return! loop()

              | Append events ->
                  do events |> writeEvents store

                  return! loop()
            }

          loop ()
        )

      {
        Get = fun () ->  agent.PostAndReply Get
        GetStream = fun eventSource -> agent.PostAndReply (fun reply -> (eventSource,reply) |> GetStream)
        Append = Append >> agent.Post
        OnError = agent.OnError
      }


module EventStore =

  type Msg<'Event> =
    | Get of AsyncReplyChannel<EventEnvelope<'Event> list>
    | GetStream of EventSource * AsyncReplyChannel<EventEnvelope<'Event> list>
    | Append of  EventEnvelope<'Event> list
    | Subscribe of EventListener<'Event>

  let notifyEventListeners events subscriptions =
    subscriptions
    |> List.iter (fun subscription -> events |> List.iter subscription)

  let initialize (onError: InfrastructureError -> unit) (storage : EventStorage<_>) : EventStore<_> =
    let handleResult success failure result =
      match result with
      | Ok result -> result |> success
      | Result.Error error -> error |> failure

    let agent =
      Agent<Msg<_>>.Start(fun inbox ->
        let rec loop (eventListeners : EventListener<'Event> list) =
          async {
            let! msg = inbox.Receive()


            match msg with
            | Get reply ->
                storage.Get()
                |> handleResult reply.Reply onError

                return! loop eventListeners

            | GetStream (source,reply) ->
                source
                |> storage.GetStream
                |> handleResult reply.Reply onError

                return! loop eventListeners

            | Append events ->
                do events |> storage.Append
                do eventListeners |> notifyEventListeners events

                return! loop eventListeners

            | Subscribe listener ->
                do
                  storage.Get()
                  |> handleResult (List.iter listener) onError

                return! loop (listener :: eventListeners)
          }

        loop []
      )

    let getStream eventSource =
      agent.PostAndReply (fun reply -> (eventSource,reply) |> GetStream)

    let append events =
      events
      |> Append
      |> agent.Post

    let subscribe (subscription : EventListener<_>) =
      subscription
      |> Subscribe
      |> agent.Post

    {
      Get = fun () ->  agent.PostAndReply Get
      GetStream = getStream
      Append = append
      Subscribe = subscribe
      OnError = agent.OnError
    }


module CommandHandler =

  let private asEvents eventEnvelopes =
    eventEnvelopes
    |> List.map (fun envelope -> envelope.Event)

  let private enveloped source events =
    events
    |> List.map (fun event -> { Source = source ; Event = event })

  type Msg<'Command> =
    | Handle of 'Command

  let initialize (behaviour : Behaviour<_,_>) (eventStore : EventStore<_>) : CommandHandler<_> =
    let agent =
      Agent<Msg<_>>.Start(fun inbox ->
        let rec loop () =
          async {
            let! msg = inbox.Receive()


            match msg with
            | Handle (eventSource,command) ->
                eventSource
                |> eventStore.GetStream
                |> asEvents
                |> behaviour command
                |> enveloped eventSource
                |> eventStore.Append

                return! loop ()
          }

        loop ()
      )

    {
      Handle = fun source command -> (source,command) |> Handle |> agent.Post
      OnError = agent.OnError
    }

module QueryHandler =

  type Msg<'Query,'Result> =
    | Query of 'Query * AsyncReplyChannel<QueryResult<'Result>>
    | AddHandler of QueryHandler<'Query, 'Result>

  let rec private choice (queryHandler : QueryHandler<_,_> list) query =
    match queryHandler with
    | handler :: rest ->
        match handler.Handle query with
        | NotHandled ->
            choice rest query

        | Handled response ->
            Handled response

    | _ -> NotHandled

  let initialize () : QueryHandler<_,_> * (QueryHandler<_,_> -> unit) =
    let agent =
      Agent<Msg<_,_>>.Start(fun inbox ->
        let rec loop queryHandler =
          async {
            let! msg = inbox.Receive()

            match msg with
            | Query (query,reply)->
                choice queryHandler query
                |> reply.Reply

                return! loop queryHandler

            | AddHandler handler ->
                return! loop (handler :: queryHandler)
          }

        loop []
      )

    let queryHandler : QueryHandler<_,_> =
      {
        Handle = fun query -> agent.PostAndReply(fun reply -> Query (query,reply))
      }


    let addQueryHandler =
      AddHandler >> agent.Post

    queryHandler,addQueryHandler





  (*
    Event Storage -> Fehler zu EventStore. Wie damit umgehen? Wenn Exception dann? Ist selber MB, also dürfen wir hier auch nicht
    verschlucken

    Command Handler kaputt -> ich bin kaputt

    EventHandler kaputt -> ich bin kaputt




    TODO:
      * Checken was passiert, wenn mit Agent passiert, wenn der einen Agent aufruft und hier eine Exception auftritt
      * MBs abbrechen oder nicht bei Exception?
      * Wie mit Results umgehen (serialize,deserialize), soll hier eine Exception geworfen werden oder nicht?
      * Idee: Ich kann auf das OnError der EventStorage im EventStore hören


  *)


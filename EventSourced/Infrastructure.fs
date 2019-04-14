namespace Infrastructure
open System

type EventSource = System.Guid

type EventProducer<'Event> =
  'Event list -> 'Event list

type EventEnvelope<'Event> =
  {
    Source : EventSource
    DateUtc : DateTime
    Event : 'Event
  }

type EventHandler<'Event> =
  EventEnvelope<'Event> list -> Async<unit>

type EventResult<'Event> =
  Result<EventEnvelope<'Event> list, string>

type EventStore<'Event> =
  {
    Get : unit -> Async<EventResult<'Event>>
    GetStream : EventSource -> Async<EventResult<'Event>>
    Append : EventEnvelope<'Event> list -> Async<Result<unit, string>>
    OnError : IEvent<exn>
    OnEvents : IEvent<EventEnvelope<'Event> list>
  }

type EventListener<'Event> =
  {
    Subscribe : EventHandler<'Event> -> unit
    Notify : EventEnvelope<'Event> list -> unit
  }

type EventStorage<'Event> =
  {
    Get : unit -> Async<EventResult<'Event>>
    GetStream : EventSource -> Async<EventResult<'Event>>
    Append : EventEnvelope<'Event> list -> Async<unit>
  }

type Projection<'State,'Event> =
  {
    Init : 'State
    Update : 'State -> 'Event -> 'State
  }

type QueryResult =
  | Handled of obj
  | NotHandled
  | QueryError of string

type  QueryHandler<'Query> =
  {
    Handle : 'Query -> Async<QueryResult>
  }

type ReadModel<'Event, 'State> =
  {
    EventHandler : EventHandler<'Event>
    State : unit -> Async<'State>
  }

type CommandHandler<'Command> =
  {
    Handle : EventSource -> 'Command -> Async<Result<unit,string>>
    OnError : IEvent<exn>
  }

type Behaviour<'Command,'Event> =
  'Command -> EventProducer<'Event>


type DB_Connection_String = DB_Connection_String of string

type EventSourcedConfig<'Comand,'Event,'Query> =
  {
    EventStoreInit : EventStorage<'Event> -> EventStore<'Event>
    EventStorageInit : unit -> EventStorage<'Event>
    CommandHandlerInit : EventStore<'Event> -> CommandHandler<'Comand>
    QueryHandler : QueryHandler<'Query>
    EventListenerInit : unit -> EventListener<'Event>
    EventHandlers : EventHandler<'Event> list
  }

type EventSourced<'Comand,'Event,'Query> (configuration : EventSourcedConfig<'Comand,'Event,'Query>) =

  let eventStorage = configuration.EventStorageInit()

  let eventStore = configuration.EventStoreInit eventStorage

  let commandHandler = configuration.CommandHandlerInit eventStore

  let queryHandler = configuration.QueryHandler

  let eventListener = configuration.EventListenerInit()

  do
    eventStore.OnError.Add(fun exn -> UI.Helper.printError (sprintf "EventStore Error: %s" exn.Message) exn)
    commandHandler.OnError.Add(fun exn -> UI.Helper.printError (sprintf "CommandHandler Error: %s" exn.Message) exn)
    eventStore.OnEvents.Add eventListener.Notify
    configuration.EventHandlers |> List.iter eventListener.Subscribe

  member __.HandleCommand eventSource command =
    commandHandler.Handle eventSource command

  member __.HandleQuery query =
    queryHandler.Handle query

  member __.GetAllEvents () =
    eventStore.Get()

  member __.GetStream eventSource =
    eventStore.GetStream eventSource


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

  member __.Trigger exn = errorEvent.Trigger exn

  /// Starts the mailbox processor
  member __.Start() = inbox.Start()
  /// Receive a message from the mailbox processor
  member __.Receive() = inbox.Receive()
  /// Post a message to the mailbox processor
  member __.Post(value:'T) = inbox.Post value

  member __.PostAndReply(f: AsyncReplyChannel<'a> -> 'T) = inbox.PostAndReply f

  member __.PostAndAsyncReply(f: AsyncReplyChannel<'a> -> 'T) = inbox.PostAndAsyncReply f

  /// Start the mailbox processor
  static member Start f =
    let agent = new Agent<_>(f)
    agent.Start()
    agent


module EventStorage =
  type Msg<'Event> =
    private
    | Get of AsyncReplyChannel<EventResult<'Event>>
    | GetStream of EventSource * AsyncReplyChannel<EventResult<'Event>>
    | Append of EventEnvelope<'Event> list * AsyncReplyChannel<unit>

  module InMemoryStorage =

    let private streamFor source history =
      history |> List.filter (fun ee -> ee.Source = source)

    let initialize () : EventStorage<'Event> =
      let history : EventEnvelope<'Event> list = []

      let proc (inbox : Agent<Msg<_>>) =
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

            | Append (events,reply) ->
                reply.Reply ()
                return! loop (history @ events)
          }

        loop history

      let agent = Agent<Msg<_>>.Start(proc)

      {
        Get = fun () ->  agent.PostAndAsyncReply Get
        GetStream = fun eventSource -> agent.PostAndAsyncReply (fun reply -> (eventSource,reply) |> GetStream)
        Append = fun events -> agent.PostAndAsyncReply (fun reply -> (events,reply) |> Append)
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

    let private get store =
      store
      |> File.ReadLines
      |> Seq.traverseResult Decode.Auto.fromString<EventEnvelope<'Event>>
      |> Result.map List.ofSeq

    let private getStream store source =
      store
      |> File.ReadLines
      |> Seq.traverseResult Decode.Auto.fromString<EventEnvelope<'Event>>
      |> Result.map (Seq.filter (fun ee -> ee.Source = source) >> List.ofSeq)

    let private append store events =
      use streamWriter = new StreamWriter(store, true)
      events
      |> List.map (fun eventEnvelope -> Encode.Auto.toString(0,eventEnvelope))
      |> List.iter streamWriter.WriteLine

      do streamWriter.Flush()


    let initialize store : EventStorage<_> =
      {
        Get = fun () -> async { return get store }
        GetStream = fun eventSource -> async { return getStream store eventSource  }
        Append = fun events -> async { return append store events }
      }

module EventStore =

  type Msg<'Event> =
    | Get of AsyncReplyChannel<EventResult<'Event>>
    | GetStream of EventSource * AsyncReplyChannel<EventResult<'Event>>
    | Append of EventEnvelope<'Event> list * AsyncReplyChannel<Result<unit,string>>

  let initialize (storage : EventStorage<_>) : EventStore<_> =
    let eventsAppended = Event<EventEnvelope<_> list>()

    let proc (inbox : Agent<Msg<_>>) =
      let rec loop (eventListeners : EventListener<_> list) =
        async {
          match! inbox.Receive() with
          | Get reply ->
              try
                let! events = storage.Get()
                do events |> reply.Reply
              with exn ->
                do inbox.Trigger(exn)
                do exn.Message |> Error |> reply.Reply

              return! loop eventListeners


          | GetStream (source,reply) ->
              try
                let! stream = source |> storage.GetStream
                do stream |> reply.Reply
              with exn ->
                do inbox.Trigger(exn)
                do exn.Message |> Error |> reply.Reply

              return! loop eventListeners

          | Append (events,reply) ->
              try
                do! events |> storage.Append
                do eventsAppended.Trigger events
                do reply.Reply (Ok ())
              with exn ->
                do inbox.Trigger(exn)
                do exn.Message |> Error |> reply.Reply

              return! loop eventListeners
        }

      loop []

    let agent =  Agent<Msg<_>>.Start(proc)

    {
      Get = fun () -> agent.PostAndAsyncReply Get
      GetStream = fun eventSource -> agent.PostAndAsyncReply (fun reply -> GetStream (eventSource,reply))
      Append = fun events -> agent.PostAndAsyncReply (fun reply -> Append (events,reply))
      OnError = agent.OnError
      OnEvents = eventsAppended.Publish
    }

module EventListener =

  type Msg<'Event> =
    | Notify of EventEnvelope<'Event> list
    | Subscribe of EventHandler<'Event>


  let notifyEventHandlers events (handlers : EventHandler<_> list) =
    handlers
    |> List.map (fun subscription -> events |> subscription )
    |> Async.Parallel
    |> Async.Ignore

  let initialize () : EventListener<_> =

    let proc (inbox : Agent<Msg<_>>) =
      let rec loop (eventHandlers : EventHandler<'Event> list) =
        async {
          match! inbox.Receive() with
          | Notify events ->
              do! eventHandlers |> notifyEventHandlers events

              return! loop eventHandlers

          | Subscribe listener ->
              return! loop (listener :: eventHandlers)
        }

      loop []

    let agent = Agent<Msg<_>>.Start(proc)

    {
      Notify = Notify >> agent.Post
      Subscribe = Subscribe >> agent.Post
    }


module CommandHandler =

  let private asEvents eventEnvelopes =
    eventEnvelopes |> List.map (fun envelope -> envelope.Event)

  let private enveloped source events =
    let now = System.DateTime.UtcNow
    let envelope event =
      {
          Source = source
          DateUtc = now
          Event = event
      }

    events |> List.map envelope

  type Msg<'Command> =
    | Handle of EventSource * 'Command * AsyncReplyChannel<Result<unit,string>>

  let initialize (behaviour : Behaviour<_,_>) (eventStore : EventStore<_>) : CommandHandler<_> =
    let proc (inbox : Agent<Msg<_>>) =
      let rec loop () =
        async {
          let! msg = inbox.Receive()

          match msg with
          | Handle (eventSource,command,reply) ->
              let! stream = eventSource |> eventStore.GetStream

              let newEvents =
                stream |> Result.map (asEvents >> behaviour command >> enveloped eventSource)

              let! result =
                newEvents
                |> function
                    | Ok events -> eventStore.Append events
                    | Error err -> async { return Error err }

              do reply.Reply result

              return! loop ()
        }

      loop ()

    let agent = Agent<Msg<_>>.Start(proc)

    {
      Handle = fun source command -> agent.PostAndAsyncReply (fun reply -> Handle (source,command,reply))
      OnError = agent.OnError
    }

module QueryHandler =
  let rec private choice (queryHandler : QueryHandler<_> list) query =
    async {
      match queryHandler with
      | handler :: rest ->
          match! handler.Handle query with
          | NotHandled ->
              return! choice rest query

          | Handled response ->
              return Handled response

          | QueryError response ->
              return QueryError response

      | _ -> return NotHandled
    }

  let initialize queryHandlers : QueryHandler<_> =
   {
      Handle = choice queryHandlers
   }

  (*
    Event Storage -> Fehler zu EventStore. Wie damit umgehen? Wenn Exception dann? Ist selber MB, also dürfen wir hier auch nicht
    verschlucken

    Command Handler kaputt -> ich bin kaputt

    EventHandler kaputt -> ich bin kaputt




    TODO:
      * Checken was passiert, wenn mit Agent passiert, wenn der einen Agent aufruft und hier eine Exception auftritt -> geht kaputt
      * MBs abbrechen oder nicht bei Exception? -> nein
      * Wie mit Results umgehen (serialize,deserialize), soll hier eine Exception geworfen werden oder nicht? -> nein
      * Idee: Ich kann auf das OnError der EventStorage im EventStore hören -> eventStorage ist nicht mehr MB
      * Idee: Result + Error Event für Exception -> erledigt
      * Idee: EventStorage nicht als Agent -> erledigt

      * Auf Query Result Async hören -> erledigt
      * QueryHandler müssen ein Result zurückgeben -> erledigt
      * QueryHandler nicht mehr Agent -> erledigt
      * EventStorage Async -> erledigt
      * EventStore Async -> erledigt
      * Alles gibt ein Ergebnis zurück. Zum Beispiel auch Append
      * Gedanken was muss Async sein, was blockend, was Agent
      * Exceptions vs Results
      * Error Messaging
          -> wir bleiben bei dem Agent, da so dass system nicht stehen bleibt, bei einer Exception
          -> wir bleiben erstmal bei einfacher Fehlerausgabe
      * Subscribe: FromNow (z.B. Live Stream aller incoming events, elmish app), FromX (persistent readmodel), FromBeginning (memory readmodel)
      * Persistent Readmodel (sold flavours über alle trucks)
          speichere current state in db
          beim Start: hole currentState aus DB
          alles andere bleibt wie es ist

          Alternative:
          hole State bei jeder Query aus DB

          Alternative:
          nur schreiben, lesen geschieht im Query Handler <- habe dies gemacht


      * Events als observable?
          dann bräuchte man erstmal keine Position!


      * nochmal Gedanken zu messaging (commandHandler ohne result oder mit?)
      * tests

      Motivation für Persistent Readmodels:
      * Server Start
      * Memory Consumption
      * Another Service that should consume the Readmodel (should not be depending on server running)



      könnten eventHandler fire and forget machen als mbp aber dann ist es nicht explizit, dass sie async sind



    Danach: Domain nicht sehr komplex: Lets use the SafeConf Planner



   ACHTUNG: Programm stürzt ab bei Commands


   erwähnen:
   man könnte correlations einfügen

  *)






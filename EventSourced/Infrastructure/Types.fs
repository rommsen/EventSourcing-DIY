namespace Infrastructure
open System

type EventSource = System.Guid

type EventProducer<'Event> =
  'Event list -> 'Event list

type EventMetadata =
  {
    Source : EventSource
    RecordedAtUtc : DateTime
  }

type EventEnvelope<'Event> =
  {
    Metadata : EventMetadata
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
    eventStore.OnError.Add(fun exn -> Helper.printError (sprintf "EventStore Error: %s" exn.Message) exn)
    commandHandler.OnError.Add(fun exn -> Helper.printError (sprintf "CommandHandler Error: %s" exn.Message) exn)
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
   event Position
   position in stream
   optimisti locking


   wenn man die DB hat, braucht man eine Event Position zum abholen
   erstmal ohne dann mit


   eventposition braucht man, wenn man db eventstore hat



   Todo:
    Menu mit untermenüs
    Storages in eigenen Ordner
    Funktionen zum Erstellen der Tables


  *)






namespace Infrastructure

type EventSource = System.Guid

type EventProducer<'Event> =
  'Event list -> 'Event list

type EventStore<'Event> =
  {
    Get : unit -> Map<EventSource,'Event list>
    GetStream : EventSource -> 'Event list
    Append : EventSource -> 'Event list -> unit
    Evolve : EventSource -> EventProducer<'Event> -> unit
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
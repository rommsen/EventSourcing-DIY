namespace Infrastructure

type EventSource = System.Guid

type EventProducer<'Event> =
  'Event list -> 'Event list


type EventMetadata =
  {
    Source : EventSource
    RecordedAtUtc : System.DateTime
  }

type EventEnvelope<'Event> =
  {
    Metadata : EventMetadata
    Event : 'Event
  }

type EventResult<'Event> =
  Result<EventEnvelope<'Event> list, string>

type EventStore<'Event> =
  {
    Get : unit -> Async<EventResult<'Event>>
    GetStream : EventSource -> Async<EventResult<'Event>>
    Append : EventEnvelope<'Event> list -> Async<Result<unit, string>>
    Evolve : EventSource -> EventProducer<'Event> -> Async<Result<unit, string>>
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
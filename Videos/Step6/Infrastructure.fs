namespace Infrastructure

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
    Append : EventSource -> 'Event list -> unit
    Evolve : EventSource -> EventProducer<'Event> -> unit
    Subscribe : EventListener<'Event>-> unit
  }

type Projection<'State,'Event> =
  {
    Init : 'State
    Update : 'State -> 'Event -> 'State
  }

type QueryResult<'Result> =
  | Handled of obj
  | NotHandled

type QueryHandler<'Query,'Result> =
  'Query -> QueryResult<'Result>



type ReadModel<'Event, 'Query, 'Result> =
  {
    EventListener : EventListener<'Event>
    QueryHandler : QueryHandler<'Query,'Result>
  }


module EventStore =
  type Msg<'Event> =
    | Get of AsyncReplyChannel<EventEnvelope<'Event> list>
    | GetStream of EventSource * AsyncReplyChannel<EventEnvelope<'Event> list>
    | Append of  EventSource * 'Event list
    | Evolve of EventSource * EventProducer<'Event>
    | Subscribe of EventListener<'Event>

  let streamFor source history =
    history
    |> List.filter (fun envelope -> envelope.Source = source)

  let asEvents eventEnvelopes =
    eventEnvelopes
    |> List.map (fun envelope -> envelope.Event)

  let enveloped source events =
    events
    |> List.map (fun event -> { Source = source ; Event = event })

  let notifyEventListeners events subscriptions =
    subscriptions
    |> List.iter (fun subscription -> events |> List.iter subscription)

  let initialize () : EventStore<'Event> =
    let history : EventEnvelope<'Event> list = []

    let mailbox =
      MailboxProcessor.Start(fun inbox ->
        let rec loop (history,eventListeners : EventListener<'Event> list) =
          async {
            let! msg = inbox.Receive()

            match msg with
            | Get reply ->
                reply.Reply history
                return! loop (history,eventListeners)

            | GetStream (source,reply) ->
                history
                |> streamFor source
                |> reply.Reply

                return! loop (history,eventListeners)

            | Append (source,events)  ->
                return! loop (history @ (events |> enveloped source), eventListeners)

            | Evolve (source,producer) ->
                let source_history =
                  history |> streamFor source

                let new_events =
                  source_history
                  |> asEvents
                  |> producer
                  |> enveloped source

                do eventListeners |> notifyEventListeners new_events

                return! loop (history @ new_events, eventListeners)

            | Subscribe listener ->
                do history |> List.iter listener

                return! loop (history, listener :: eventListeners)

                // Idee: gib möglichkeiten subscribern zum slicen mit

                // Frage wohin wir optimieren wollen?
                // Zugriffe auf Streams?
                // alle Events
                // Memory?
                // es h#ngt davon ab wohin man optimieren möchte

                // wenn alle Events dann möchte man sie schon in Order haben

                // Eine Event Subscription braucht sie auf jeden Fall in der Order

                // achtung Prototype: eventuell structs

          }

        loop (history,[])
      )

    let getStream aggregate =
      mailbox.PostAndReply (fun reply -> (aggregate,reply) |> GetStream)

    let append aggregate events =
      (aggregate,events)
      |> Append
      |> mailbox.Post

    let evolve aggregate producer =
      (aggregate,producer)
      |> Evolve
      |> mailbox.Post

    let subscribe (subscription : EventListener<_>) =
      subscription
      |> Subscribe
      |> mailbox.Post

    {
      Get = fun () ->  mailbox.PostAndReply Get
      GetStream = getStream
      Append = append
      Evolve = evolve
      Subscribe = subscribe
    }


module QueryHandler =

  type Msg<'Query,'Result> =
    | Query of 'Query * AsyncReplyChannel<QueryResult<'Result>>
    | AddHandler of QueryHandler<'Query, 'Result>

  let rec private choice (queryHandler : QueryHandler<_,_> list) query =
    match queryHandler with
    | handler :: rest ->
        match handler query with
        | NotHandled ->
            choice rest query

        | Handled response ->
            Handled response

    | _ -> NotHandled

  let initialize () : QueryHandler<_,_> * (QueryHandler<_,_> -> unit) =
    let agent =
      MailboxProcessor.Start(fun inbox ->
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

    let queryHandler query =
      agent.PostAndReply(fun reply -> Query (query,reply))

    let addQueryHandler =
      AddHandler >> agent.Post

    queryHandler,addQueryHandler

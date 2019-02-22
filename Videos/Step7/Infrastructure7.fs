namespace Step7.Infrastructure

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


type CommandHandler<'Command> =
  {
    Handle : EventSource -> 'Command -> unit
  }

type Behaviour<'Command,'Event> =
  'Command -> EventProducer<'Event>


module EventStore =

  type Msg<'Event> =
    | Get of AsyncReplyChannel<EventEnvelope<'Event> list>
    | GetStream of EventSource * AsyncReplyChannel<EventEnvelope<'Event> list>
    | Append of  EventEnvelope<'Event> list
    | Subscribe of EventListener<'Event>

  let streamFor source history =
    history
    |> List.filter (fun envelope -> envelope.Source = source)

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

            | Append events ->
                do  eventListeners |> notifyEventListeners events

                return! loop (history @ events, eventListeners)

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

    let getStream eventSource =
      mailbox.PostAndReply (fun reply -> (eventSource,reply) |> GetStream)

    let append events =
      events
      |> Append
      |> mailbox.Post

    let subscribe (subscription : EventListener<_>) =
      subscription
      |> Subscribe
      |> mailbox.Post

    {
      Get = fun () ->  mailbox.PostAndReply Get
      GetStream = getStream
      Append = append
      Subscribe = subscribe
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
      MailboxProcessor.Start(fun inbox ->
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

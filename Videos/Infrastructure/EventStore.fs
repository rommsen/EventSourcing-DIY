namespace Infrastructure

module EventStore =

  type Msg<'Event> =
    | Get of AsyncReplyChannel<EventResult<'Event>>
    | GetStream of EventSource * AsyncReplyChannel<EventResult<'Event>>
    | Append of EventEnvelope<'Event> list * AsyncReplyChannel<Result<unit,string>>
    | Evolve of EventSource * EventProducer<'Event> *AsyncReplyChannel<Result<unit,string>>

  let private enveloped source events =
    let now = System.DateTime.UtcNow
    let envelope event =
      {
          Metadata = {
            Source = source
            RecordedAtUtc = now
          }
          Event = event
      }

    events |> List.map envelope

  let initialize (storage : EventStorage<_>) : EventStore<_> =

    let proc (inbox : MailboxProcessor<Msg<_>>) =
      let rec loop () =
        async {
          match! inbox.Receive() with
          | Get reply ->
              let! events = storage.Get()
              do events |> reply.Reply

              return! loop ()

          | GetStream (source,reply) ->
              let! stream = source |> storage.GetStream
              do stream |> reply.Reply

              return! loop ()

          | Append (events,reply) ->
              do! events |> storage.Append
              do reply.Reply (Ok ())

              return! loop ()

          | Evolve (source,producer,reply) ->
              match! source |> storage.GetStream with
              | Ok stream ->
                  let events =
                    stream
                    |> List.map (fun envelope -> envelope.Event)
                    |> producer

                  do! events |> enveloped source |> storage.Append

                  do reply.Reply (Ok ())

              | Error error ->
                  do reply.Reply (Error error)

              return! loop () // TODO try/with
        }

      loop ()

    let agent =  MailboxProcessor<Msg<_>>.Start(proc)

    {
      Get = fun () -> agent.PostAndAsyncReply Get
      GetStream = fun eventSource -> agent.PostAndAsyncReply (fun reply -> GetStream (eventSource,reply))
      Append = fun events -> agent.PostAndAsyncReply (fun reply -> Append (events,reply))
      Evolve = fun eventSource producer -> agent.PostAndAsyncReply (fun reply -> Evolve (eventSource,producer,reply))
    }
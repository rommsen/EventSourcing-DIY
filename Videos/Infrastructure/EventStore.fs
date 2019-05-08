namespace Infrastructure

module EventStore =
  type Msg<'Event> =
  | Get of AsyncReplyChannel<Map<EventSource,'Event list>>
  | GetStream of EventSource * AsyncReplyChannel<'Event list>
  | Append of  EventSource * 'Event list
  | Evolve of EventSource * EventProducer<'Event>

  let initialize () : EventStore<'Event> =
    let history : Map<EventSource,'Event> = Map.empty

    let mailbox =
      MailboxProcessor.Start(fun inbox ->
        let rec loop history =
          async {
            let! msg = inbox.Receive()

            match msg with
            | Get reply ->
                reply.Reply history
                return! loop history

            | GetStream (eventSource,reply) ->
                history
                |> Map.tryFind eventSource
                |> Option.defaultValue []
                |> reply.Reply

                return! loop history

            | Append (eventSource,events)  ->
                let stream_history =
                  history
                  |> Map.tryFind eventSource
                  |> Option.defaultValue []

                return! loop (
                    history
                    |> Map.add eventSource (stream_history @ events))

            | Evolve (eventSource,producer) ->
                let stream_history =
                  history
                  |> Map.tryFind eventSource
                  |> Option.defaultValue []

                let events =
                  stream_history
                  |> producer

                return! loop (
                    history
                    |> Map.add eventSource (stream_history @ events)
                )
          }

        loop history
      )

    let getStream eventSource =
      mailbox.PostAndReply (fun reply -> (eventSource,reply) |> GetStream)

    let append eventSource events =
      (eventSource,events)
      |> Append
      |> mailbox.Post

    let evolve eventSource producer =
      (eventSource,producer)
      |> Evolve
      |> mailbox.Post

    {
      Get = fun () ->  mailbox.PostAndReply Get
      GetStream = getStream
      Append = append
      Evolve = evolve
    }
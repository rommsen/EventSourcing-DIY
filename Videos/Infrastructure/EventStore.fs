namespace Infrastructure

module EventStore =
  type Msg<'Event> =
  | Get of AsyncReplyChannel<Map<Aggregate,'Event list>>
  | GetStream of Aggregate * AsyncReplyChannel<'Event list>
  | Append of  Aggregate * 'Event list
  | Evolve of Aggregate * EventProducer<'Event>

  let initialize () : EventStore<'Event> =
    let history : Map<Aggregate,'Event> = Map.empty

    let mailbox =
      MailboxProcessor.Start(fun inbox ->
        let rec loop history =
          async {
            let! msg = inbox.Receive()

            match msg with
            | Get reply ->
                reply.Reply history
                return! loop history

            | GetStream (aggregate,reply) ->
                history
                |> Map.tryFind aggregate
                |> Option.defaultValue []
                |> reply.Reply

                return! loop history

            | Append (aggregate,events)  ->
                let stream_history =
                  history
                  |> Map.tryFind aggregate
                  |> Option.defaultValue []

                return! loop (
                    history
                    |> Map.add aggregate (stream_history @ events))

            | Evolve (aggregate,producer) ->
                let stream_history =
                  history
                  |> Map.tryFind aggregate
                  |> Option.defaultValue []

                let events =
                  stream_history
                  |> producer

                return! loop (
                    history
                    |> Map.add aggregate (stream_history @ events)
                )
          }

        loop history
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

    {
      Get = fun () ->  mailbox.PostAndReply Get
      GetStream = getStream
      Append = append
      Evolve = evolve
    }
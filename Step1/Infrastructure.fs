namespace Step1.Infrastructure

type Events<'Event> =
  'Event list

type EventProducer<'Event> =
  'Event list -> 'Event list

type EventStore<'Event> =
  {
    Get : unit -> Events<'Event>
    Append : Events<'Event> -> unit
  }


module EventStore =
  type Msg<'Event> =
    | Get of AsyncReplyChannel<'Event list>
    | Append of Events<'Event>

  let init () : EventStore<'Event> =
    let history = []

    let mailbox =
      MailboxProcessor.Start(fun inbox ->
        let rec loop history =
            async {
              let! msg = inbox.Receive()

              match msg with
              | Append events  ->
                  return! loop (history @ events)

              | Get reply ->
                  reply.Reply history
                  return! loop history
            }

        loop history
      )

    let append events =
      events
      |> Append
      |> mailbox.Post

    {
      Append = append
      Get = fun () ->  mailbox.PostAndReply Get
    }
namespace Step1.Infrastructure

type Events<'Event> =
  'Event list

type EventStore<'Event> =
  {
    Get : unit -> Events<'Event>
    Append : Events<'Event> -> unit
  }

type Projection<'State,'Event> =
  {
    Init : 'State
    Update : 'State -> 'Event -> 'State
  }


module EventStore =

  type Msg<'Event> =
    | Get of AsyncReplyChannel<'Event list>
    | Append of Events<'Event>

  let initialize () : EventStore<'Event> =
    let history = []

    let mailbox =
      MailboxProcessor.Start(fun inbox ->
        let rec loop history =
          async {
            let! msg = inbox.Receive()

            match msg with
            | Get reply ->
                reply.Reply history
                return! loop history

            | Append events  ->
                return! loop (history @ events)
          }

        loop history
      )

    let append events =
      events
      |> Append
      |> mailbox.Post

    {
      Get = fun () ->  mailbox.PostAndReply Get
      Append = append
    }
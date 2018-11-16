module Infrastructure =

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


module Domain =

  type Flavour =
    | Vanilla
    | Strawberry

  type Event =
    | Flavour_sold of Flavour
    | Flavour_restocked of Flavour * int
    | Flavour_went_out_of_stock of Flavour
    | Flavour_was_not_in_stock of Flavour



open Infrastructure
open Domain

[<EntryPoint>]
let main _ =

  let eventStore : EventStore<Event> = EventStore.initialize()

  eventStore.Append [Flavour_restocked (Vanilla,42)]
  eventStore.Append [Flavour_went_out_of_stock Vanilla]

  let events = eventStore.Get()

  printfn "\n\n\nEvents:\n %A\n\n\n" events

  0
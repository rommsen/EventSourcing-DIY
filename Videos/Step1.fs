module Infrastructure =

  type EventStore<'Event> =
    {
      Get : unit -> 'Event list
      Append : 'Event list -> unit
    }


   module EventStore =

    type Msg<'Event> =
      | Get of AsyncReplyChannel<'Event list>
      | Append of 'Event list

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


module Helper =
  let printUl list =
    list
    |> List.iteri (fun i item -> printfn " %i: %A" (i+1) item)

  let printEvents  events =
    events
    |> List.length
    |> printfn "History (Length: %i)"

    events |> printUl


open Infrastructure
open Domain
open Helper

[<EntryPoint>]
let main _ =

  let eventStore : EventStore<Event> = EventStore.initialize()

  eventStore.Append [Flavour_restocked (Vanilla,3)]

  eventStore.Append [Flavour_sold Vanilla]
  eventStore.Append [Flavour_sold Vanilla]
  eventStore.Append [Flavour_sold Vanilla ; Flavour_went_out_of_stock Vanilla]

  eventStore.Get()
  |> printEvents

  0





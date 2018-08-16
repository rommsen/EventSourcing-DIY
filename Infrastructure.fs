module Infrastructure


type Projector<'State,'Event> = 'State -> 'Event -> 'State

// einfache Projektion
type Projection<'State,'Event> =
  {
    InitialState : 'State
    UpdateState : Projector<'State,'Event>
  }


type EventStore<'Event> =
  {
    Append : 'Event list -> unit
    Get :  unit -> 'Event list
  }



// später Get für Stream
// später: warte aufs Commit
type Msg<'Event> =
  | Get of AsyncReplyChannel<'Event list>
  | Append of 'Event list

let eventStore () : EventStore<'Event> =
  let events = []

  let mailbox =
    MailboxProcessor.Start(fun inbox ->
      // printfn "Start"
      let rec loop events =
          async {
            let! msg = inbox.Receive()

            match msg with
            | Append newEvents ->
                return! loop (List.concat [ events ; newEvents ])

            | Get reply ->
                reply.Reply events
                return! loop events
          }

      loop events
    )

  let append events =
    mailbox.Post (Append events)

  {
    Append = append
    Get = fun () ->  mailbox.PostAndReply(Get)
  }



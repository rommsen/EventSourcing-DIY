module Infrastructure


type Projector<'State,'Event> = 'State -> 'Event -> 'State

// einfache Projektion
type Projection<'State,'Event> =
  {
    InitialState : 'State
    UpdateState : Projector<'State,'Event>
  }

// Erst mit Append versuchen zeigen?


type EventProducer<'Event> =
  'Event list -> 'Event list

type EventStore<'Event> =
  {
    Run : EventProducer<'Event> -> unit
    Get : unit -> 'Event list
  }



// später Get für Stream
// später: warte aufs Commit
type Msg<'Event> =
  | Get of AsyncReplyChannel<'Event list>
  | Run of EventProducer<'Event>

let eventStore () : EventStore<'Event> =
  let events = []

  let mailbox =
    MailboxProcessor.Start(fun inbox ->
      // printfn "Start"
      let rec loop events =
          async {
            let! msg = inbox.Receive()

            match msg with
            | Run (producer)  ->
                return! loop (List.concat [ events ; producer events ])

            | Get reply ->
                reply.Reply events
                return! loop events
          }

      loop events
    )

  let run producer =
    mailbox.Post (Run producer)

  {
    Run = run
    Get = fun () ->  mailbox.PostAndReply Get
  }



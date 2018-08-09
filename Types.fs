module Types


type Event =
  | Event1
  | Event2
  | Event3

// einfache Projektion
type Projection<'State,'Event> =
  {
    InitialState : 'State
    UpdateState : 'State -> 'Event list -> 'State
  }


type EventStore<'Event> = {
  Append : 'Event list -> unit
  Get :  unit -> 'Event list
}


  // 2 Beispiele in der Domäne

  // wie werden aus Events State

  // einfach Funktionen, kein Subscriber. Wir müssen also die Events im State halten und abfragen

  // Aktion holt sich auch den Stream und lässt die Projektion durchlaufen. Aber welchen Stream?

// brauchen möglichkeit State zu speichern, vllt ein Objekt?
// Aktor erklären
// Mailbox Prozessor erklären
// brauchen eine Antwort

// zeige Type Driven: erst später den Event Store implementieren

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
                // printfn "EventStore received new events: %A" eventSet
                return! loop (List.concat [ events ; newEvents ])

            | Get reply ->
                reply.Reply events

                return! loop events
          }

      loop events
    )

  let read() =
    async {
      return! mailbox.PostAndAsyncReply(Get)
    } |> Async.RunSynchronously


  let append events =
    events |> Append |> mailbox.Post

  {
    Append = append
    Get = read
  }





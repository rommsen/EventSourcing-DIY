module Infrastructure


type Projector<'State,'Event> = 'State -> 'Event -> 'State

// einfache Projektion
type Projection<'State,'Event> =
  {
    Init : 'State
    Update : Projector<'State,'Event>
  }

// Erst mit Append versuchen zeigen?
type Aggregate = System.Guid

type Events<'Event> =
  Map<Aggregate,'Event list>

type EventProducer<'Event> =
  'Event list -> 'Event list

type EventStore<'Event> =
  {
    Evolve : Aggregate -> EventProducer<'Event> -> unit
    Get : unit -> Events<'Event>
    GetStream : Aggregate -> 'Event list
  }

// type Msg<'Event> =
//   | Get of AsyncReplyChannel<'Event list>
//   | Run of EventProducer<'Event>

// let eventStore () : EventStore<'Event> =
//   let events = []

//   let mailbox =
//     MailboxProcessor.Start(fun inbox ->
//       // printfn "Start"
//       let rec loop host =
//           async {
//             let! msg = inbox.Receive()

//             match msg with
//             | Run producer  ->
//                 return! loop (List.concat [ events ; producer events ])

//             | Get reply ->
//                 reply.Reply events
//                 return! loop events
//           }

//       loop events
//     )

//   let run producer =
//     mailbox.Post (Run producer)

//   {
//     Run = run
//     Get = fun () ->  mailbox.PostAndReply Get
//   }

// später Get für Stream
// später: warte aufs Commit
type Msg<'Event> =
  | Get of AsyncReplyChannel<Events<'Event>>
  | GetStream of Aggregate * AsyncReplyChannel<'Event list>
  | Evolve of Aggregate * EventProducer<'Event>

let initializeEventStore () : EventStore<'Event> =
  let history : Map<Aggregate,'Event> = Map.empty                           // maybe List,Dict is faster but build what you need

  let mailbox =
    MailboxProcessor.Start(fun inbox ->
      // printfn "Start"
      let rec loop history =
          async {
            let! msg = inbox.Receive()

            match msg with
            | Evolve (aggregate,producer)  ->
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

            | Get reply ->
                reply.Reply history
                return! loop history

            | GetStream (aggregate,reply) ->
                history
                |> Map.tryFind aggregate
                |> Option.defaultValue []
                |> reply.Reply

                return! loop history
          }

      loop history
    )

  let evolve aggregate producer =
    (aggregate,producer)
    |> Evolve
    |> mailbox.Post

  let getStream aggregate =
    mailbox.PostAndReply(fun reply -> (aggregate,reply) |> GetStream)

  {
    Evolve = evolve
    Get = fun () ->  mailbox.PostAndReply Get
    GetStream = getStream

  }



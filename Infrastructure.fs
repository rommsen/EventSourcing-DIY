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
    Run : Aggregate -> EventProducer<'Event> -> unit
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
//       let rec loop events =
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
  | Run of Aggregate * EventProducer<'Event>

let eventStore () : EventStore<'Event> =
  let events : Map<Aggregate,'Event> = Map.empty                           // maybe List,Dict is slower but build what you need

  let mailbox =
    MailboxProcessor.Start(fun inbox ->
      // printfn "Start"
      let rec loop events =
          async {
            let! msg = inbox.Receive()

            match msg with
            | Run (aggregate,producer)  ->
                let events_for_stream =
                  events
                  |> Map.tryFind aggregate
                  |> Option.defaultValue []

                let new_events =
                  events_for_stream
                  |> producer

                return! loop (
                    events
                    |> Map.add aggregate (List.concat [ events_for_stream ; new_events ])
                )

            | Get reply ->
                reply.Reply events
                return! loop events

            | GetStream (aggregate,reply) ->
                events
                |> Map.tryFind aggregate
                |> Option.defaultValue []
                |> reply.Reply

                return! loop events
          }

      loop events
    )

  let run aggregate producer =
    (aggregate,producer)
    |> Run
    |> mailbox.Post

  let getStream aggregate =
    mailbox.PostAndReply(fun reply -> (aggregate,reply) |> GetStream)

  {
    Run = run
    Get = fun () ->  mailbox.PostAndReply Get
    GetStream = getStream

  }



module Step2.Program

open Step2.Domain
open Step2.Infrastructure

let eventStore : EventStore<Event> = EventStore.initialize()

type Msg =
  | AppendFlavour_soldVanilla
  | AppendFlavour_soldStrawberry
  | AppendFlavour_soldStrawberryFlavourEmptyStrawberry
  | GetEvents of AsyncReplyChannel<Event list>
  | SoldIcecreams of AsyncReplyChannel<Flavour list>

let mailbox () =
  let eventStore : EventStore<Event> = EventStore.initialize()

  MailboxProcessor.Start(fun inbox ->
    let rec loop eventStore =
      async {
        let! msg = inbox.Receive()

        match msg with
        | AppendFlavour_soldVanilla ->
            eventStore.Append [Flavour_sold Vanilla]
            return! loop eventStore

        | AppendFlavour_soldStrawberry ->
            eventStore.Append [Flavour_sold Strawberry ]
            return! loop eventStore

        | AppendFlavour_soldStrawberryFlavourEmptyStrawberry ->
            eventStore.Append [Flavour_sold Strawberry ; Flavour_empty Strawberry]
            return! loop eventStore

        | GetEvents reply ->
            reply.Reply (eventStore.Get())
            return! loop eventStore

        | SoldIcecreams reply ->
            eventStore.Get()
            |> List.fold Projections.soldIcecreams.Update Projections.soldIcecreams.Init
            |> reply.Reply

            return! loop eventStore
      }

    loop eventStore
  )


let appendFlavour_soldVanilla (mailbox : MailboxProcessor<Msg>) =
  mailbox.Post Msg.AppendFlavour_soldVanilla

let appendFlavour_soldStrawberry (mailbox : MailboxProcessor<Msg>) =
  mailbox.Post Msg.AppendFlavour_soldStrawberry

let appendFlavour_soldStrawberryFlavourEmptyStrawberry (mailbox : MailboxProcessor<Msg>) =
  mailbox.Post Msg.AppendFlavour_soldStrawberryFlavourEmptyStrawberry

let getEvents (mailbox : MailboxProcessor<Msg>) =
  mailbox.PostAndReply Msg.GetEvents

let listOfSoldFlavours (mailbox : MailboxProcessor<Msg>) =
  mailbox.PostAndReply Msg.SoldIcecreams

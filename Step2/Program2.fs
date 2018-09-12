namespace Step2

module Program =

  open Step2.Domain
  open Step2.Infrastructure

  let eventStore : EventStore<Event> = EventStore.initialize()

  type Msg =
    | Append_Flavour_sold_Vanilla
    | Append_Flavour_sold_Strawberry
    | Append_Flavour_sold_StrawberryFlavourEmptyStrawberry
    | GetEvents of AsyncReplyChannel<Event list>
    | SoldIcecreams of AsyncReplyChannel<Flavour list>

  let mailbox () =
    let eventStore : EventStore<Event> = EventStore.initialize()

    MailboxProcessor.Start(fun inbox ->
      let rec loop eventStore =
        async {
          let! msg = inbox.Receive()

          match msg with
          | Append_Flavour_sold_Vanilla ->
              eventStore.Append [Flavour_sold Vanilla]
              return! loop eventStore

          | Append_Flavour_sold_Strawberry ->
              eventStore.Append [Flavour_sold Strawberry ]
              return! loop eventStore

          | Append_Flavour_sold_StrawberryFlavourEmptyStrawberry ->
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


  let Append_Flavour_sold_Vanilla (mailbox : MailboxProcessor<Msg>) =
    mailbox.Post Msg.Append_Flavour_sold_Vanilla

  let Append_Flavour_sold_Strawberry (mailbox : MailboxProcessor<Msg>) =
    mailbox.Post Msg.Append_Flavour_sold_Strawberry

  let Append_Flavour_sold_StrawberryFlavourEmptyStrawberry (mailbox : MailboxProcessor<Msg>) =
    mailbox.Post Msg.Append_Flavour_sold_StrawberryFlavourEmptyStrawberry

  let getEvents (mailbox : MailboxProcessor<Msg>) =
    mailbox.PostAndReply Msg.GetEvents

  let listOfSoldFlavours (mailbox : MailboxProcessor<Msg>) =
    mailbox.PostAndReply Msg.SoldIcecreams

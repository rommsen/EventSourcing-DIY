namespace Step1

module Program =

  open Step1.Domain
  open Step1.Infrastructure

  let eventStore : EventStore<Event> = EventStore.initialize()

  type Msg =
    | Append_Flavour_sold_Vanilla
    | Append_Flavour_sold_Strawberry
    | Append_Flavour_sold_StrawberryFlavourEmptyStrawberry
    | GetEvents of AsyncReplyChannel<Event list>
    | SoldFlavours of AsyncReplyChannel<Map<Flavour,int>>

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
              eventStore.Append [Flavour_sold Strawberry ; Flavour_went_out_of_stock Strawberry]
              return! loop eventStore

          | GetEvents reply ->
              reply.Reply (eventStore.Get())
              return! loop eventStore

          | SoldFlavours reply ->
              eventStore.Get()
              |> List.fold Projections.soldFlavours.Update Projections.soldFlavours.Init
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
    mailbox.PostAndReply Msg.SoldFlavours

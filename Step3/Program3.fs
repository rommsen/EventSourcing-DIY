namespace Step3

module Program =

  open Step3.Domain
  open Step3.Infrastructure

  type Msg =
    | DemoData
    | SellFlavour of Flavour
    | GetEvents of AsyncReplyChannel<Event list>
    | SoldFlavours of AsyncReplyChannel<Flavour list>

  let mailbox () =
    let eventStore : EventStore<Event> = EventStore.initialize()

    MailboxProcessor.Start(fun inbox ->
      let rec loop eventStore =
        async {
          let! msg = inbox.Receive()

          match msg with
          | DemoData ->
              eventStore.Append [Flavour_sold Vanilla]
              eventStore.Append [Flavour_sold Vanilla]
              eventStore.Append [Flavour_sold Strawberry ]
              eventStore.Append [Flavour_sold Strawberry]
              return! loop eventStore

          | SellFlavour flavour ->
              eventStore.Evolve (Behaviour.sellFlavour flavour)
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


  let demoData (mailbox : MailboxProcessor<Msg>) =
    mailbox.Post Msg.DemoData

  let sellFlavour flavour (mailbox : MailboxProcessor<Msg>) =
    mailbox.Post (Msg.SellFlavour flavour)

  let getEvents (mailbox : MailboxProcessor<Msg>) =
    mailbox.PostAndReply Msg.GetEvents

  let listOfSoldFlavours (mailbox : MailboxProcessor<Msg>) =
    mailbox.PostAndReply Msg.SoldFlavours

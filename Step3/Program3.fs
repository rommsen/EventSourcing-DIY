namespace Step3

module Program =

  open Step3.Domain
  open Step3.Infrastructure

  type Msg =
    | DemoData
    | SellIcecream of Flavour
    | GetEvents of AsyncReplyChannel<Event list>
    | SoldIcecreams of AsyncReplyChannel<Flavour list>

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

          | SellIcecream flavour ->
              eventStore.Evolve (Behaviour.sellIceCream flavour)
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


  let demoData (mailbox : MailboxProcessor<Msg>) =
    mailbox.Post Msg.DemoData

  let sellIcecream flavour (mailbox : MailboxProcessor<Msg>) =
    mailbox.Post (Msg.SellIcecream flavour)

  let getEvents (mailbox : MailboxProcessor<Msg>) =
    mailbox.PostAndReply Msg.GetEvents

  let listOfSoldFlavours (mailbox : MailboxProcessor<Msg>) =
    mailbox.PostAndReply Msg.SoldIcecreams

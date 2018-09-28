namespace Step6

module Program =

  open Step6.Domain
  open Step6.Infrastructure

  type Msg =
    | DemoData
    | SellFlavour of Flavour
    | Restock of Flavour * portions : int
    | StockOf of Flavour * AsyncReplyChannel<int>
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
              eventStore.Append [Flavour_restocked (Vanilla,5)]
              eventStore.Append [Flavour_restocked (Strawberry,2)]
              eventStore.Append [Flavour_sold Vanilla]
              eventStore.Append [Flavour_sold Vanilla]
              eventStore.Append [Flavour_sold Strawberry ]
              eventStore.Append [Flavour_sold Strawberry ; Flavour_empty Strawberry]
              return! loop eventStore

          | SellFlavour flavour ->
              eventStore.Evolve (Behaviour.sellFlavour flavour)
              return! loop eventStore

          | Restock (flavour, portions) ->
              eventStore.Evolve (Behaviour.restock flavour portions)
              return! loop eventStore

          | StockOf (flavour,reply) ->
              eventStore.Get()
              |> Projections.project Projections.flavoursInStock
              |> Projections.stockOf flavour
              |> reply.Reply

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

  let restock flavour portions (mailbox : MailboxProcessor<Msg>) =
    mailbox.Post (Msg.Restock (flavour,portions))

  let stockOf flavour (mailbox : MailboxProcessor<Msg>) =
    mailbox.PostAndReply(fun reply -> Msg.StockOf (flavour,reply))

  let getEvents (mailbox : MailboxProcessor<Msg>) =
    mailbox.PostAndReply Msg.GetEvents

  let listOfSoldFlavours (mailbox : MailboxProcessor<Msg>) =
    mailbox.PostAndReply Msg.SoldFlavours
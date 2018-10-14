namespace Step4

module Program =

  open Step4.Domain
  open Step4.Infrastructure

  let truck1 = System.Guid.Parse "a6c993b0-7ec4-4e48-8324-768746183f30"
  let truck2 = System.Guid.Parse "9a96e338-e55d-4ab2-88e9-cf1852906a26"

  type Msg =
    | DemoData
    | SellFlavour of Aggregate * Flavour
    | Restock of Aggregate * Flavour * portions : int
    | StockOf of Aggregate * Flavour * AsyncReplyChannel<int>
    | GetEvents of AsyncReplyChannel<Events<Event>>
    | GetEventStream of Aggregate * AsyncReplyChannel<Event list>
    | SoldFlavours of Aggregate * AsyncReplyChannel<Map<Flavour,int>>
    | TrucksWithSoldNumberOfFlavours of AsyncReplyChannel<Map<Aggregate,int>>

  let mailbox () =
    let eventStore : EventStore<Event> = EventStore.initialize()

    MailboxProcessor.Start(fun inbox ->
      let rec loop eventStore =
        async {
          let! msg = inbox.Receive()

          match msg with
          | DemoData ->
              eventStore.Append truck1 [Flavour_restocked (Vanilla,5)]
              eventStore.Append truck1 [Flavour_restocked (Strawberry,2)]
              eventStore.Append truck1 [Flavour_sold Vanilla]
              eventStore.Append truck1 [Flavour_sold Vanilla]
              eventStore.Append truck1 [Flavour_sold Strawberry ]
              eventStore.Append truck1 [Flavour_sold Strawberry ; Flavour_went_out_of_stock Strawberry]

              eventStore.Append truck2 [Flavour_restocked (Vanilla,2)]
              eventStore.Append truck2 [Flavour_restocked (Strawberry,5)]
              eventStore.Append truck2 [Flavour_sold Vanilla]
              eventStore.Append truck2 [Flavour_sold Strawberry ]

              return! loop eventStore

          | SellFlavour (truck,flavour) ->
              eventStore.Evolve truck (Behaviour.sellFlavour flavour)
              return! loop eventStore

          | Restock (truck, flavour, portions) ->
              eventStore.Evolve truck (Behaviour.restock flavour portions)
              return! loop eventStore

          | StockOf (truck,flavour,reply) ->
              truck
              |> eventStore.GetStream
              |> Projections.project Projections.flavoursInStock
              |> Projections.stockOf flavour

              |> reply.Reply

              return! loop eventStore

          | GetEvents reply ->
              reply.Reply (eventStore.Get())
              return! loop eventStore

          | SoldFlavours (truck,reply) ->
              truck
              |> eventStore.GetStream
              |> Projections.project Projections.soldFlavours
              |> reply.Reply

              return! loop eventStore

          | GetEventStream (truck,reply) ->
              truck
              |> eventStore.GetStream
              |> reply.Reply

              return! loop eventStore

          | TrucksWithSoldNumberOfFlavours reply ->
              eventStore.Get()
              |> Map.map (fun _ events ->
                    events
                    |> Projections.project Projections.soldFlavours
                    |> Map.toList
                    |> List.length)
              |> reply.Reply

              return! loop eventStore
        }

      loop eventStore
    )


  let demoData (mailbox : MailboxProcessor<Msg>) =
    mailbox.Post Msg.DemoData

  let sellFlavour truck flavour (mailbox : MailboxProcessor<Msg>) =
    mailbox.Post (Msg.SellFlavour (truck,flavour))

  let restock truck flavour portions (mailbox : MailboxProcessor<Msg>) =
    mailbox.Post (Msg.Restock (truck,flavour,portions))

  let stockOf truck flavour (mailbox : MailboxProcessor<Msg>) =
    mailbox.PostAndReply(fun reply -> Msg.StockOf (truck,flavour,reply))

  let getEvents (mailbox : MailboxProcessor<Msg>) =
    mailbox.PostAndReply Msg.GetEvents

  let getEventStream truck (mailbox : MailboxProcessor<Msg>) =
    mailbox.PostAndReply(fun reply -> Msg.GetEventStream (truck,reply))

  let listOfSoldFlavours truck (mailbox : MailboxProcessor<Msg>) =
    mailbox.PostAndReply(fun reply -> Msg.SoldFlavours (truck,reply))

  let trucksWithSoldNumberOfFlavours (mailbox : MailboxProcessor<Msg>) =
    mailbox.PostAndReply Msg.TrucksWithSoldNumberOfFlavours
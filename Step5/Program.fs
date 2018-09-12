module Step5.Program

open Step5.Domain
open Step5.Infrastructure

type Msg =
  | DemoData
  | SellIcecream of Flavour
  | Restock of Flavour * portions : int
  | StockOf of Flavour * AsyncReplyChannel<int>
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
            eventStore.Append [Flavour_restocked (Vanilla,5)]
            eventStore.Append [Flavour_restocked (Strawberry,2)]
            eventStore.Append [Flavour_sold Vanilla]
            eventStore.Append [Flavour_sold Vanilla]
            eventStore.Append [Flavour_sold Strawberry ]
            eventStore.Append [Flavour_sold Strawberry ; Flavour_empty Strawberry]
            return! loop eventStore

        | SellIcecream flavour ->
            eventStore.Evolve (Behaviour.sellIceCream flavour)
            return! loop eventStore

        | Restock (flavour, portions) ->
            eventStore.Evolve (Behaviour.restock flavour portions)
            return! loop eventStore

        | StockOf (flavour,reply) ->
            eventStore.Get()
            |> Projections.project Projections.icecreamsInStock
            |> Projections.stockOf flavour
            |> reply.Reply

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

let restock flavour portions (mailbox : MailboxProcessor<Msg>) =
  mailbox.Post (Msg.Restock (flavour,portions))

let stockOf flavour (mailbox : MailboxProcessor<Msg>) =
  mailbox.PostAndReply(fun reply -> Msg.StockOf (flavour,reply))

let getEvents (mailbox : MailboxProcessor<Msg>) =
  mailbox.PostAndReply Msg.GetEvents

let listOfSoldFlavours (mailbox : MailboxProcessor<Msg>) =
  mailbox.PostAndReply Msg.SoldIcecreams
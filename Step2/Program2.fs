namespace Step2

open Step2.Domain
module Program =

  open Step2.Domain
  open Step2.Infrastructure

  type Msg =
    | DemoData
    | SellFlavour of Flavour
    | Restock of Flavour * portions : int
    | StockOf of Flavour * AsyncReplyChannel<int>
    | GetEvents of AsyncReplyChannel<Event list>
    | SoldFlavours of AsyncReplyChannel<Map<Flavour,int>>

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
              eventStore.Append [Flavour_sold Strawberry ; Flavour_went_out_of_stock Strawberry]
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


// events
// type Flavour =
//   | Vanilla
//   | Strawberry

// type Event =
//   | Flavour_sold of Flavour
//   | Flavour_restocked of Flavour * int
//   | Flavour_went_out_of_stock of Flavour
//   | Flavour_was_not_in_stock of Flavour

// let event = Flavour_sold Vanilla

// eventStore1
// type EventStore<'Event> =
//   {
//     Get    : unit -> 'Event list
//     Append : 'Event list -> unit
//   }

// eventStore2
// type EventStore<'Event> =
//   {
//     Get    : unit -> Events<'Event>
//     Append : Events<'Event> -> unit
//   }

// eventStore3
// type EventProducer<'Event> =
//   'Event list -> 'Event list

// type EventStore<'Event> =
//   {
//     Get    : unit -> 'Event list
//     Append : 'Event list -> unit
//     Evolve : EventProducer<'Event> -> unit
//   }




// EventStore2a
// type Event =
//   | Flavour_sold of Flavour
//   // ...

// module Program =

//   let sellFlavour flavour (eventStore : EventStore<Event>) =
//     eventStore.Append [Flavour_sold flavour]

//   [<EntryPoint>]
//   let main _ =

//     let eventStore = // initialize EventStore

//     sellFlavour Vanilla eventStore

// projections1
// type Projection<'State,'Event> =
//   {
//     Init   : 'State
//     Update : 'State -> 'Event -> 'State
//   }

// projections2
// let private updateSoldFlavours state event =
//   match event with
//   | Flavour_sold flavour ->
//       state
//       |> Map.tryFind flavour
//       |> Option.defaultValue 0
//       |> fun portions ->
//           state |> Map.add flavour (portions + 1)

//   | _ ->
//       state

// let soldFlavours : Projection<Map<Flavour,int>, Event> =
//   {
//     Init = Map.empty
//     Update = updateSoldFlavours
//   }

// let listOfSoldFlavours events =
//   events |> List.fold soldFlavours.Update soldFlavours.Init

// let soldFlavours : Projection<Flavour list, Event> =
//   {
//     Init = []
//     Update = updateSoldFlavours
//   }

// Projections3
// let updateSoldFlavours state event =
//   match event with
//   | Flavour_sold flavour ->
//       flavour :: state

//   | _ ->
//       state

// let soldFlavours : Projection<Flavour list, Event> =
//   {
//     Init = []
//     Update = updateSoldFlavours
//   }

// let listOfSoldFlavours events =
//   events |> List.fold soldFlavours.Update soldFlavours.Init

// // projections4
// let project (projection : Projection<_,_>) events =
//   events |> List.fold projection.Update projection.Init

// let soldFlavours : Projection<Flavour list, Event> =
//   {
//     Init = []
//     Update = updateSoldFlavours
//   }

// let listOfSoldFlavours events =
//   events |> project soldFlavours


// businessLogic1
// module Behaviour =

//   let sellFlavour flavour : EventProducer<Event> =
//     fun events ->
//       let stock =
//           //  do something with events that returns current stock of flavour

//       match stock with
//       | 0 -> [Flavour_was_not_in_stock flavour]
//       | 1 -> [Flavour_sold flavour ; Flavour_went_out_of_stock flavour]
//       | _ -> [Flavour_sold flavour]


// module Program =

//   let sellFlavour flavour (eventStore : EventStore<Event>) =
//     eventStore.Evolve (Behaviour.sellFlavour flavour)

//   [<EntryPoint>]
//   let main _ =

//     let eventStore = // initialize EventStore

//     sellFlavour Vanilla eventStore


// business_Logic2
// module Behaviour =

//   let private stockOf flavour stock =
//     stock
//     |> Map.tryFind flavour
//     |> Option.defaultValue 0

//   let sellFlavour flavour : EventProducer<Event> =
//     fun events ->
//       let stock =
//         events
//         |> List.fold flavoursInStock.Update flavoursInStock.Init
//         |> stockOf flavour

//       match stock with
//       | 0 -> [Flavour_was_not_in_stock flavour]
//       | 1 -> [Flavour_sold flavour ; Flavour_went_out_of_stock flavour]
//       | _ -> [Flavour_sold flavour]




// module Projections =

//   let private updateFlavoursInStock stock event =
//     match event with
//     | Flavour_sold flavour ->
//         stock
//         |> Map.tryFind flavour
//         |> Option.map (fun portions ->
//             stock |> Map.add flavour (portions - 1))
//         |> Option.defaultValue stock

//     | _ ->
//         stock


//   let flavoursInStock : Projection<Map<Flavour, int>, Event> =
//     {
//       Init = Map.empty
//       Update = updateFlavoursInStock
//     }

// Tests
// test "Flavour_sold" {
//   Given
//     [
//       Flavour_restocked (Vanilla,2)
//       Flavour_sold Vanilla
//     ]
//   |> When (Behaviour.sell_Flavour Vanilla)
//   |> Then [Flavour_sold Vanilla ; Flavour_went_out_of_stock Vanilla]
// }


// business logic

// restock_falsch
// let restock flavour number stock =
//   stock
//   |> Map.tryFind flavour
//   |> Option.map (fun portions -> stock |> Map.add flavour (portions + number))
//   |> Option.defaultValue stock


// restock_richtig
// let restock flavour number stock =
//   stock
//   |> Map.tryFind flavour
//   |> Option.defaultValue 0
//   |> fun portions -> stock |> Map.add flavour (portions + number)
module Helper =

  open Expecto
  open Step7.Domain
  open Step7.Tests

  let printUl list =
    list
    |> List.iteri (fun i item -> printfn " %i: %A" (i+1) item)

  let printEvents header events =
    events
    |> List.length
    |> printfn "History for %s (Length: %i)" header

    events |> printUl


  let printTotalHistory history =
    history
    |> List.length
    |> printfn "Total History Length: %i"


  let printSoldFlavour flavour state =
    state
    |> Projections.soldOfFlavour flavour
    |> printfn "Sold %A: %i" flavour

  let printStockOf flavour state =
    state
    |> Projections.stockOf flavour
    |> printfn "Stock of %A: %i" flavour


  let runTests () =
    runTests defaultConfig Domain.domainTests |> ignore



open Step7.Infrastructure
open Step7.Application
open Step7.Domain
open Helper

[<EntryPoint>]
let main _ =

  // runTests ()

  let guid (Truck guid) = guid

  let truck1 = Truck <| System.Guid.Parse "49d9d107-aceb-4b2d-a7e3-eca784a9de6e"
  let truck2 = Truck <| System.Guid.Parse "8b916bde-6bdf-43cc-b43b-69c9f4c3e5c4"

  let eventStore : EventStore<Event> =
    EventStore.initialize()

  let commandHandler : CommandHandler<Command> =
    CommandHandler.initialize Behaviour.behaviour eventStore

  let queryHandler,addQueryHandler =
    QueryHandler.initialize()

  let readmodels =
    [
      Readmodels.flavoursInStock()
      Readmodels.trucks()
    ]

  readmodels
  |> List.iter (fun readmodel ->
      do readmodel.EventListener |> eventStore.Subscribe
      do readmodel.QueryHandler |> addQueryHandler)


  let truck1_guid = guid truck1
  let truck2_guid = guid truck2

  Sell_flavour (truck1, Vanilla) |> commandHandler.Handle truck1_guid
  Sell_flavour (truck1, Strawberry) |> commandHandler.Handle truck1_guid
  Restock_flavour (truck1, Vanilla, 5) |> commandHandler.Handle truck1_guid
  Sell_flavour (truck1, Vanilla) |> commandHandler.Handle truck1_guid

  Restock_flavour (truck2, Strawberry, 5) |> commandHandler.Handle truck2_guid
  Sell_flavour (truck2, Strawberry) |> commandHandler.Handle truck2_guid
  Sell_flavour (truck2, Strawberry) |> commandHandler.Handle truck2_guid
  Sell_flavour (truck2, Strawberry) |> commandHandler.Handle truck2_guid

  System.Console.WriteLine "Eventual Consistency in place. Hit enter to go on."
  System.Console.ReadLine() |> ignore

  let events_truck_1 = eventStore.GetStream truck1_guid
  let events_truck_2 = eventStore.GetStream truck2_guid

  events_truck_1 |> printEvents "Truck 1"
  events_truck_2 |> printEvents "Truck 2"

  // eventStore.Get() |> printEvents "all"


  let flavourResult = queryHandler (API.Query.FlavoursInStock (truck1, Vanilla))
  printfn "flavourResult %A" flavourResult

  let truckResult = queryHandler API.Query.Trucks
  printfn "truckresult %A" truckResult
  0
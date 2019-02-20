module Helper =

  open Expecto
  open Domain
  open Tests

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



open Infrastructure
open Application
open Domain
open Helper

[<EntryPoint>]
let main _ =

  let guid (Truck guid) = guid

  let truck1 = Truck <| System.Guid.NewGuid()
  let truck2 = Truck <| System.Guid.NewGuid()

  runTests ()

  let eventStore : EventStore<Event> = EventStore.initialize()
  let queryHandler,addQueryHandler = QueryHandler.initialize()
  let readmodels =
    [
      Readmodels.flavoursInStock()
      Readmodels.trucks()
    ]

  readmodels
  |> List.iter (fun readmodel ->
      do readmodel.EventListener |> eventStore.Subscribe
      do readmodel.QueryHandler |> addQueryHandler)


  eventStore.Evolve (guid truck1) (Behaviour.sellFlavour truck1 Vanilla)
  eventStore.Evolve (guid truck1) (Behaviour.sellFlavour truck1 Strawberry)
  eventStore.Evolve (guid truck1) (Behaviour.restock truck1 Vanilla 5)
  eventStore.Evolve (guid truck1) (Behaviour.sellFlavour truck1 Vanilla)

  eventStore.Evolve (guid truck2) (Behaviour.restock truck2 Strawberry 3)
  eventStore.Evolve (guid truck2) (Behaviour.sellFlavour truck2 Strawberry)
  eventStore.Evolve (guid truck2) (Behaviour.sellFlavour truck2 Strawberry)
  eventStore.Evolve (guid truck2) (Behaviour.sellFlavour truck2 Strawberry)

  let events_truck_1 = eventStore.GetStream (guid truck1)
  let events_truck_2 = eventStore.GetStream (guid truck2)

  events_truck_1 |> printEvents "Truck 1"
  events_truck_2 |> printEvents "Truck 2"

  eventStore.Get()
  |> printTotalHistory


  let flavourResult = queryHandler (API.Query.FlavoursInStock (truck1, Vanilla))
  printfn "flavourResult %A" flavourResult

  let truckResult = queryHandler API.Query.Trucks
  printfn "truckresult %A" truckResult
  0
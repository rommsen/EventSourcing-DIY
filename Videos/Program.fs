module Helper =

  open Expecto
  open Step8.Domain
  open Step8.Tests

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



open Step8.Infrastructure
open Step8.Application
open Step8.Domain
open Helper

[<EntryPoint>]
let main _ =

  // runTests ()

  let guid (Truck guid) = guid

  let truck1 = Truck <| System.Guid.Parse "49d9d107-aceb-4b2d-a7e3-eca784a9de6e"
  let truck2 = Truck <| System.Guid.Parse "8b916bde-6bdf-43cc-b43b-69c9f4c3e5c4"

  let eventStore : EventStore<Event> =
    @"C:\temp\store.txt"
    |> EventStorage.FileStorage.initialize
    |> EventStore.initialize


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


  let main =
    [
      ("Total History", eventStore.Get >> printEvents "all" >> UI.Helper.waitForAnyKey)
      ("History Truck 1", fun () -> truck1_guid |> eventStore.GetStream |> printEvents "Truck 1" |>  UI.Helper.waitForAnyKey)
      ("History Truck 2", fun () -> truck2_guid |> eventStore.GetStream |> printEvents "Truck 2" |>  UI.Helper.waitForAnyKey)
      ("Query.FlavoursInStock (truck1, Vanilla)", fun () -> API.Query.FlavoursInStock (truck1, Vanilla) |> queryHandler |> printfn "Stock Truck 1 Vanilla: %A" |> UI.Helper.waitForAnyKey)
      ("Query.FlavoursInStock (truck2, Vanilla)", fun () -> API.Query.FlavoursInStock (truck2, Vanilla) |> queryHandler |> printfn "Stock Truck 2 Vanilla %A" |> UI.Helper.waitForAnyKey)
      ("Query.FlavoursInStock (truck1, Strawberry)", fun () -> API.Query.FlavoursInStock (truck1, Strawberry) |> queryHandler |> printfn "Stock Truck 1 Strawberry: %A" |> UI.Helper.waitForAnyKey)
      ("Query.FlavoursInStock (truck2, Strawberry)", fun () -> API.Query.FlavoursInStock (truck2, Strawberry) |> queryHandler |> printfn "Stock Truck 2 Strawberry %A" |> UI.Helper.waitForAnyKey)
      ("Sell_flavour (truck1, Vanilla)", fun () -> Sell_flavour (truck1, Vanilla) |> commandHandler.Handle truck1_guid)
      ("Sell_flavour (truck2, Vanilla)", fun () -> Sell_flavour (truck2, Vanilla) |> commandHandler.Handle truck2_guid)
      ("Sell_flavour (truck1, Strawberry)", fun () -> Sell_flavour (truck1, Strawberry) |> commandHandler.Handle truck1_guid)
      ("Sell_flavour (truck2, Strawberry)", fun () -> Sell_flavour (truck2, Strawberry) |> commandHandler.Handle truck2_guid)
      ("Restock_flavour (truck1, Vanilla, 5)", fun () -> Restock_flavour (truck1, Vanilla, 5) |> commandHandler.Handle truck1_guid)
      ("Restock_flavour (truck2, Vanilla, 5)", fun () -> Restock_flavour (truck2, Vanilla, 5) |> commandHandler.Handle truck2_guid)
      ("Restock_flavour (truck1, Strawberry, 5)", fun () -> Restock_flavour (truck1, Strawberry, 5) |> commandHandler.Handle truck1_guid)
      ("Restock_flavour (truck2, Strawberry, 5)", fun () -> Restock_flavour (truck2, Strawberry, 5) |> commandHandler.Handle truck2_guid)
    ], ignore

  main
  |> UI.Menu.initialize () "Event Sourcing DIY"
  |> ignore

  0
open Infrastructure
module Helper =

  open Expecto
  open Domain
  open Tests

  let printUl list =
    list
    |> List.iteri (fun i item -> printfn " %i: %A" (i+1) item)

  let printEvents header events =
    match events with
    | Ok events ->
        events
        |> List.length
        |> printfn "\nHistory for %s (Length: %i)" header

        events |> printUl

    | Error error -> UI.Helper.printError (sprintf "Error when retrieving events: %s" error) ""

    UI.Helper.waitForAnyKey()

  let printCommandResults header result =
    match result with
    | Ok _ ->
        printfn "\n%s: %A" header result

    | Error error ->
        UI.Helper.printError (sprintf "Command Error: %s" error) ""

    UI.Helper.waitForAnyKey()



  let printQueryResults header result =
    match result with
    | QueryResult.Handled result ->
        printfn "\n%s: %A" header result

    | QueryResult.NotHandled ->
        printfn "\n%s: NOT HANDLED" header

    | QueryResult.QueryError error ->
        UI.Helper.printError (sprintf "Query Error: %s" error) ""

    UI.Helper.waitForAnyKey()

  let printSoldFlavour flavour state =
    state
    |> Projections.soldOfFlavour flavour
    |> printfn "\nSold %A: %i" flavour

  let printStockOf flavour state =
    state
    |> Projections.stockOf flavour
    |> printfn "\nStock of %A: %i" flavour

  let runAsync asnc =
    asnc |> Async.RunSynchronously


  let runTests () =
    runTests defaultConfig Domain.domainTests |> ignore



open Application
open Domain
open API
open Helper

[<EntryPoint>]
let main _ =

  // runTests ()

  let guid (Truck guid) = guid

  let truck1 = Truck <| System.Guid.Parse "49d9d107-aceb-4b2d-a7e3-eca784a9de6e"
  let truck2 = Truck <| System.Guid.Parse "8b916bde-6bdf-43cc-b43b-69c9f4c3e5c4"

  let flavoursReadmodel = InMemoryReadmodels.flavoursInStock()
  let queryHandlers =
    [
      QueryHandlers.flavours flavoursReadmodel.State
    ]

  let eventListener =
    [
      flavoursReadmodel.EventListener
    ]

  let app =
    EventSourced<Command,Event,API.Query,obj>(
      EventStore.initialize,
      (fun () -> @"C:\temp\store.txt" |> EventStorage.FileStorage.initialize),
      CommandHandler.initialize Behaviour.behaviour,
      QueryHandler.initialize queryHandlers,
      eventListener
    )

  let truck1_guid = guid truck1
  let truck2_guid = guid truck2

  let main =
    [
      ("Total History", fun () -> app.GetAllEvents() |> runAsync |> printEvents "all")
      ("History Truck 1", fun () -> truck1_guid |> app.GetStream |> runAsync |> printEvents "Truck 1")
      ("History Truck 2", fun () -> truck2_guid |> app.GetStream |> runAsync |> printEvents "Truck 2")
      ("Query.FlavoursInStock (truck1, Vanilla)", fun () -> FlavourInStockOfTruck (truck1, Vanilla) |> app.HandleQuery |> runAsync |> printQueryResults "Stock Truck 1 Vanilla")
      ("Query.FlavoursInStock (truck2, Vanilla)", fun () -> FlavourInStockOfTruck (truck2, Vanilla) |> app.HandleQuery |> runAsync |> printQueryResults "Stock Truck 2 Vanilla")
      ("Query.FlavoursInStock (truck1, Strawberry)", fun () -> FlavourInStockOfTruck (truck1, Strawberry) |> app.HandleQuery |> runAsync |> printQueryResults "Stock Truck 1 Strawberry")
      ("Query.FlavoursInStock (truck2, Strawberry)", fun () -> FlavourInStockOfTruck (truck2, Strawberry) |> app.HandleQuery |> runAsync |> printQueryResults "Stock Truck 2 Strawberry")
      ("Sell_flavour (truck1, Vanilla)", fun () -> Sell_flavour (truck1, Vanilla) |> app.HandleCommand truck1_guid |> runAsync |> printCommandResults "Command")
      ("Sell_flavour (truck2, Vanilla)", fun () -> Sell_flavour (truck2, Vanilla) |> app.HandleCommand truck2_guid |> runAsync |> printCommandResults "Command")
      ("Sell_flavour (truck1, Strawberry)", fun () -> Sell_flavour (truck1, Strawberry) |> app.HandleCommand truck1_guid |> runAsync |> printCommandResults "Command")
      ("Sell_flavour (truck2, Strawberry)", fun () -> Sell_flavour (truck2, Strawberry) |> app.HandleCommand truck2_guid |> runAsync |> printCommandResults "Command")
      ("Restock_flavour (truck1, Vanilla, 5)", fun () -> Restock_flavour (truck1, Vanilla, 5) |> app.HandleCommand truck1_guid |> runAsync |> printCommandResults "Command")
      ("Restock_flavour (truck2, Vanilla, 5)", fun () -> Restock_flavour (truck2, Vanilla, 5) |> app.HandleCommand truck2_guid |> runAsync |> printCommandResults "Command")
      ("Restock_flavour (truck1, Strawberry, 5)", fun () -> Restock_flavour (truck1, Strawberry, 5) |> app.HandleCommand truck1_guid |> runAsync |> printCommandResults "Command")
      ("Restock_flavour (truck2, Strawberry, 5)", fun () -> Restock_flavour (truck2, Strawberry, 5) |> app.HandleCommand truck2_guid |> runAsync |> printCommandResults "Command")
    ], ignore

  main
  |> UI.Menu.initialize () "Event Sourcing DIY"
  |> ignore

  0
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
open Npgsql.FSharp

let db_connection =
  Sql.host "localhost"
  |> Sql.port 5432
  |> Sql.username "postgres"
  |> Sql.password "123456"
  |> Sql.database "EventSourced"
  |> Sql.str
  |> DB_Connection_String

let eventStoreFile = @"C:\temp\store.txt"

[<EntryPoint>]
let main _ =

  let guid (Truck guid) = guid

  let truck1 = Truck <| System.Guid.Parse "49d9d107-aceb-4b2d-a7e3-eca784a9de6e"
  let truck2 = Truck <| System.Guid.Parse "8b916bde-6bdf-43cc-b43b-69c9f4c3e5c4"

  let flavoursInStockReadmodel = InMemoryReadmodels.flavoursInStock()
  // let flavoursSoldReadmodel = InMemoryReadmodels.flavoursSold()

  let configuration =
    {
      EventStoreInit =
        EventStore.initialize

      EventStorageInit =
        (fun () -> eventStoreFile |> EventStorage.FileStorage.initialize)

      CommandHandlerInit =
        CommandHandler.initialize Behaviour.behaviour

      QueryHandler =
        QueryHandler.initialize
          [
            QueryHandlers.flavours flavoursInStockReadmodel.State db_connection
          ]

      EventListenerInit =
        EventListener.initialize

      EventHandlers =
        [
          flavoursInStockReadmodel.EventHandler
          PersistentReadmodels.flavourSoldHandler db_connection
        ]
    }

  let app = EventSourced<Command,Event,API.Query>(configuration)

  let truck1_guid = guid truck1
  let truck2_guid = guid truck2

  let main =
    [
      ("Run Tests", runTests >> UI.Helper.waitForAnyKey)
      ("Total History", fun () -> app.GetAllEvents() |> runAsync |> printEvents "all")
      ("History Truck 1", fun () -> truck1_guid |> app.GetStream |> runAsync |> printEvents "Truck 1")
      ("History Truck 2", fun () -> truck2_guid |> app.GetStream |> runAsync |> printEvents "Truck 2")
      ("Query.FlavourInStockOfTruck (truck1, Vanilla)", fun () -> FlavourInStockOfTruck (truck1, Vanilla) |> app.HandleQuery |> runAsync |> printQueryResults "Stock Truck 1 Vanilla")
      ("Query.FlavourInStockOfTruck (truck2, Vanilla)", fun () -> FlavourInStockOfTruck (truck2, Vanilla) |> app.HandleQuery |> runAsync |> printQueryResults "Stock Truck 2 Vanilla")
      ("Query.FlavourInStockOfTruck (truck1, Strawberry)", fun () -> FlavourInStockOfTruck (truck1, Strawberry) |> app.HandleQuery |> runAsync |> printQueryResults "Stock Truck 1 Strawberry")
      ("Query.FlavourInStockOfTruck (truck2, Strawberry)", fun () -> FlavourInStockOfTruck (truck2, Strawberry) |> app.HandleQuery |> runAsync |> printQueryResults "Stock Truck 2 Strawberry")
      ("Query.FlavourInStockOfAll Strawberry", fun () -> FlavourInStockOfAll Strawberry |> app.HandleQuery |> runAsync |> printQueryResults "Total Stock Strawberry")
      ("Query.FlavourInStockOfAll Vanilla", fun () -> FlavourInStockOfAll Vanilla |> app.HandleQuery |> runAsync |> printQueryResults "Total Stock Vanilla")
      ("Query.FlavoursSoldOfTruck (truck1, Vanilla)", fun () -> FlavoursSoldOfTruck (truck1, Vanilla) |> app.HandleQuery |> runAsync |> printQueryResults "Sold Truck 1 Vanilla")
      ("Query.FlavoursSoldOfTruck (truck2, Vanilla)", fun () -> FlavoursSoldOfTruck (truck2, Vanilla) |> app.HandleQuery |> runAsync |> printQueryResults "Sold Truck 2 Vanilla")
      ("Query.FlavoursSoldOfTruck (truck1, Strawberry)", fun () -> FlavoursSoldOfTruck (truck1, Strawberry) |> app.HandleQuery |> runAsync |> printQueryResults "Sold Truck 1 Strawberry")
      ("Query.FlavoursSoldOfTruck (truck2, Strawberry)", fun () -> FlavoursSoldOfTruck (truck2, Strawberry) |> app.HandleQuery |> runAsync |> printQueryResults "Sold Truck 2 Strawberry")
      ("Query.FlavoursSoldOfAll Strawberry", fun () -> FlavoursSoldOfAll Strawberry |> app.HandleQuery |> runAsync |> printQueryResults "Total Sold Strawberry")
      ("Query.FlavoursSoldOfAll Vanilla", fun () -> FlavoursSoldOfAll Vanilla |> app.HandleQuery |> runAsync |> printQueryResults "Total Sold Vanilla")
      ("Add_truck_to_fleet truck1", fun () -> Add_truck_to_fleet truck1 |> app.HandleCommand truck1_guid |> runAsync |> printCommandResults "Command")
      ("Add_truck_to_fleet truck2", fun () -> Add_truck_to_fleet truck2 |> app.HandleCommand truck1_guid |> runAsync |> printCommandResults "Command")
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
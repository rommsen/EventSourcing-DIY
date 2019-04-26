open Expecto
open Npgsql.FSharp

open Infrastructure
open EventSourced
open Domain
open Tests
open Application
open API
open Helper

let db_connection =
  Sql.host "localhost"
  |> Sql.port 5432
  |> Sql.username "postgres"
  |> Sql.password "123456"
  |> Sql.database "EventSourced"
  |> Sql.str
  |> DB_Connection_String

let eventStoreFile = @"C:\temp\store.txt"

let flavoursInStockReadmodel = InMemoryReadmodels.flavoursInStock()
  // let flavoursSoldReadmodel = InMemoryReadmodels.flavoursSold()

let configuration =
  {
    EventStoreInit =
      EventStore.initialize

    EventStorageInit =
      (fun () -> db_connection |> EventStorage.PostgresStorage.initialize)

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


let guid (Truck guid) = guid

let truck1 = Truck <| System.Guid.Parse "49d9d107-aceb-4b2d-a7e3-eca784a9de6e"
let truck2 = Truck <| System.Guid.Parse "8b916bde-6bdf-43cc-b43b-69c9f4c3e5c4"

let truck1_guid = guid truck1
let truck2_guid = guid truck2


[<EntryPoint>]
let main _ =

  let app = EventSourced<Command,Event,API.Query>(configuration)

  let utils =
    [
      ("Run Tests", fun () -> runTests defaultConfig Domain.domainTests |> ignore; waitForAnyKey())
      ("Clean Flavours Sold Readmodel", fun () -> PersistentReadmodels.clean_sold_flavours db_connection |>  printfn "Clean Result %A";  waitForAnyKey())
      ("Repopulate Flavours Sold Readmodel",  fun () -> app.GetAllEvents() |> PersistentReadmodels.repopulate_sold_flavours (PersistentReadmodels.flavourSoldHandler db_connection) |> runAsync ;  waitForAnyKey())
    ], ignore


  let history =
    [
      ("Total History", fun () -> app.GetAllEvents() |> runAsync |> printEvents "all")
      ("History Truck 1", fun () -> truck1_guid |> app.GetStream |> runAsync |> printEvents "Truck 1")
      ("History Truck 2", fun () -> truck2_guid |> app.GetStream |> runAsync |> printEvents "Truck 2")
    ], ignore

  let commands =
    [
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

  let queries =
    [
      ("FlavourInStockOfTruck (truck1, Vanilla)", fun () -> FlavourInStockOfTruck (truck1, Vanilla) |> app.HandleQuery |> runAsync |> printQueryResults "Stock Truck 1 Vanilla")
      ("FlavourInStockOfTruck (truck2, Vanilla)", fun () -> FlavourInStockOfTruck (truck2, Vanilla) |> app.HandleQuery |> runAsync |> printQueryResults "Stock Truck 2 Vanilla")
      ("FlavourInStockOfTruck (truck1, Strawberry)", fun () -> FlavourInStockOfTruck (truck1, Strawberry) |> app.HandleQuery |> runAsync |> printQueryResults "Stock Truck 1 Strawberry")
      ("FlavourInStockOfTruck (truck2, Strawberry)", fun () -> FlavourInStockOfTruck (truck2, Strawberry) |> app.HandleQuery |> runAsync |> printQueryResults "Stock Truck 2 Strawberry")
      ("FlavourInStockOfAll Strawberry", fun () -> FlavourInStockOfAll Strawberry |> app.HandleQuery |> runAsync |> printQueryResults "Total Stock Strawberry")
      ("FlavourInStockOfAll Vanilla", fun () -> FlavourInStockOfAll Vanilla |> app.HandleQuery |> runAsync |> printQueryResults "Total Stock Vanilla")
      ("FlavoursSoldOfTruck (truck1, Vanilla)", fun () -> FlavoursSoldOfTruck (truck1, Vanilla) |> app.HandleQuery |> runAsync |> printQueryResults "Sold Truck 1 Vanilla")
      ("FlavoursSoldOfTruck (truck2, Vanilla)", fun () -> FlavoursSoldOfTruck (truck2, Vanilla) |> app.HandleQuery |> runAsync |> printQueryResults "Sold Truck 2 Vanilla")
      ("FlavoursSoldOfTruck (truck1, Strawberry)", fun () -> FlavoursSoldOfTruck (truck1, Strawberry) |> app.HandleQuery |> runAsync |> printQueryResults "Sold Truck 1 Strawberry")
      ("FlavoursSoldOfTruck (truck2, Strawberry)", fun () -> FlavoursSoldOfTruck (truck2, Strawberry) |> app.HandleQuery |> runAsync |> printQueryResults "Sold Truck 2 Strawberry")
      ("FlavoursSoldOfAll Strawberry", fun () -> FlavoursSoldOfAll Strawberry |> app.HandleQuery |> runAsync |> printQueryResults "Total Sold Strawberry")
      ("FlavoursSoldOfAll Vanilla", fun () -> FlavoursSoldOfAll Vanilla |> app.HandleQuery |> runAsync |> printQueryResults "Total Sold Vanilla")
    ], ignore

  let main =
    [
      ("History", fun () -> history |> UI.Menu.initialize () "History")
      ("Commands", fun () -> commands |> UI.Menu.initialize () "Commands")
      ("Queries", fun () -> queries |> UI.Menu.initialize () "Queries")
      ("Utils", fun () -> utils |> UI.Menu.initialize () "Utils")
    ], ignore




  main
  |> UI.Menu.initialize () "Event Sourcing DIY"
  |> ignore

  0
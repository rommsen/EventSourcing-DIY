open Infrastructure
open Domain
open Projections
open Helper
open Expecto
open Application
open API

let printSoldFlavour flavour state =
  state
  |> soldOfFlavour flavour
  |> printfn "Sold %A: %i" flavour

let printStockOf flavour state =
  state
  |> stockOf flavour
  |> printfn "Stock of %A: %i" flavour

let guid (Truck guid) = guid

let truck1 = Truck <| System.Guid.Parse "49d9d107-aceb-4b2d-a7e3-eca784a9de6e"
let truck2 = Truck <| System.Guid.Parse "8b916bde-6bdf-43cc-b43b-69c9f4c3e5c4"

let truck1_guid = guid truck1
let truck2_guid = guid truck2

[<EntryPoint>]
let main _ =

  let eventStore : EventStore<Event> = EventStore.initialize()

  let queryHandler =
    QueryHandler.initialize
      [
        Application.QueryHandlers.flavours eventStore
      ]

  let utils =
    [
      ("Run Tests", fun () -> runTests defaultConfig Tests.tests |> ignore; waitForAnyKey())
    ], ignore


  let history =
    [
      ("Total History", fun () -> eventStore.Get() |> printTotalHistory ; waitForAnyKey())
      ("History Truck 1", fun () -> eventStore.GetStream truck1_guid |> printEvents "Truck 1" ; waitForAnyKey())
      ("History Truck 2", fun () -> eventStore.GetStream truck2_guid  |> printEvents "Truck 2"  ; waitForAnyKey())
    ], ignore

  let behaviour =
    [
      ("Sell_flavour (truck1, Vanilla)", fun () -> eventStore.Evolve truck1_guid (Behaviour.sellFlavour Vanilla) ; waitForAnyKey())
      ("Sell_flavour (truck2, Vanilla)", fun () -> eventStore.Evolve truck2_guid (Behaviour.sellFlavour Vanilla) ; waitForAnyKey())
      ("Sell_flavour (truck1, Strawberry)", fun () -> eventStore.Evolve truck1_guid (Behaviour.sellFlavour Strawberry) ; waitForAnyKey())
      ("Sell_flavour (truck2, Strawberry)", fun () -> eventStore.Evolve truck2_guid (Behaviour.sellFlavour Strawberry) ; waitForAnyKey())
      ("Restock_flavour (truck1, Vanilla, 5)", fun () -> eventStore.Evolve truck1_guid (Behaviour.restock Vanilla 5))
      ("Restock_flavour (truck2, Vanilla, 5)", fun () -> eventStore.Evolve truck2_guid (Behaviour.restock Vanilla 5))
      ("Restock_flavour (truck1, Strawberry, 5)", fun () -> eventStore.Evolve truck1_guid (Behaviour.restock Strawberry 5))
      ("Restock_flavour (truck2, Strawberry, 5)", fun () -> eventStore.Evolve truck2_guid (Behaviour.restock Strawberry 5))
    ], ignore

  let queries =
    [
      ("FlavourInStockOfTruck (truck1, Vanilla)", fun () -> FlavourInStockOfTruck (truck1, Vanilla) |> queryHandler.Handle |> printQueryResults "Stock Truck 1 Vanilla")
      ("FlavourInStockOfTruck (truck2, Vanilla)", fun () -> FlavourInStockOfTruck (truck2, Vanilla) |> queryHandler.Handle |> printQueryResults "Stock Truck 2 Vanilla")
      ("FlavourInStockOfTruck (truck1, Strawberry)", fun () -> FlavourInStockOfTruck (truck1, Strawberry) |> queryHandler.Handle |> printQueryResults "Stock Truck 1 Strawberry")
      ("FlavourInStockOfTruck (truck2, Strawberry)", fun () -> FlavourInStockOfTruck (truck2, Strawberry) |> queryHandler.Handle |> printQueryResults "Stock Truck 2 Strawberry")
      ("FlavourInStockOfAll Strawberry", fun () -> FlavourInStockOfAll Strawberry |> queryHandler.Handle |> printQueryResults "Total Stock Strawberry")
      ("FlavourInStockOfAll Vanilla", fun () -> FlavourInStockOfAll Vanilla |> queryHandler.Handle |> printQueryResults "Total Stock Vanilla")
      ("FlavoursSoldOfTruck (truck1, Vanilla)", fun () -> FlavoursSoldOfTruck (truck1, Vanilla) |> queryHandler.Handle |> printQueryResults "Sold Truck 1 Vanilla")
      ("FlavoursSoldOfTruck (truck2, Vanilla)", fun () -> FlavoursSoldOfTruck (truck2, Vanilla) |> queryHandler.Handle |> printQueryResults "Sold Truck 2 Vanilla")
      ("FlavoursSoldOfTruck (truck1, Strawberry)", fun () -> FlavoursSoldOfTruck (truck1, Strawberry) |> queryHandler.Handle |> printQueryResults "Sold Truck 1 Strawberry")
      ("FlavoursSoldOfTruck (truck2, Strawberry)", fun () -> FlavoursSoldOfTruck (truck2, Strawberry) |> queryHandler.Handle |> printQueryResults "Sold Truck 2 Strawberry")
      ("FlavoursSoldOfAll Strawberry", fun () -> FlavoursSoldOfAll Strawberry |> queryHandler.Handle |> printQueryResults "Total Sold Strawberry")
      ("FlavoursSoldOfAll Vanilla", fun () -> FlavoursSoldOfAll Vanilla |> queryHandler.Handle |> printQueryResults "Total Sold Vanilla")
    ], ignore

  let main =
    [
      ("History", fun () -> history |> UI.Menu.initialize () "History")
      ("Behaviour", fun () -> behaviour |> UI.Menu.initialize () "Behaviour")
      ("Queries", fun () -> queries |> UI.Menu.initialize () "Queries")
      ("Utils", fun () -> utils |> UI.Menu.initialize () "Utils")
    ], ignore




  main
  |> UI.Menu.initialize () "Event Sourcing DIY"
  |> ignore

  0
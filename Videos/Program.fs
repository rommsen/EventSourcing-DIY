open Infrastructure
open Domain
open Projections
open Helper
open Expecto

let printSoldFlavour flavour state =
  state
  |> soldOfFlavour flavour
  |> printfn "Sold %A: %i" flavour

let printStockOf flavour state =
  state
  |> stockOf flavour
  |> printfn "Stock of %A: %i" flavour

let truck1 = System.Guid.Parse "49d9d107-aceb-4b2d-a7e3-eca784a9de6e"
let truck2 = System.Guid.Parse "8b916bde-6bdf-43cc-b43b-69c9f4c3e5c4"

[<EntryPoint>]
let main _ =

  let eventStore : EventStore<Event> = EventStore.initialize()

  let utils =
    [
      ("Run Tests", fun () -> runTests defaultConfig Tests.tests |> ignore; waitForAnyKey())
    ], ignore


  let history =
    [
      ("Total History", fun () -> eventStore.Get() |> printTotalHistory ; waitForAnyKey())
      ("History Truck 1", fun () -> eventStore.GetStream truck1 |> printEvents "Truck 1" ; waitForAnyKey())
      ("History Truck 2", fun () -> eventStore.GetStream truck2  |> printEvents "Truck 2"  ; waitForAnyKey())
    ], ignore

  let behaviour =
    [
      ("Sell_flavour (truck1, Vanilla)", fun () -> eventStore.Evolve truck1 (Behaviour.sellFlavour Vanilla) ; waitForAnyKey())
      ("Sell_flavour (truck2, Vanilla)", fun () -> eventStore.Evolve truck2 (Behaviour.sellFlavour Vanilla) ; waitForAnyKey())
      ("Sell_flavour (truck1, Strawberry)", fun () -> eventStore.Evolve truck1 (Behaviour.sellFlavour Strawberry) ; waitForAnyKey())
      ("Sell_flavour (truck2, Strawberry)", fun () -> eventStore.Evolve truck2 (Behaviour.sellFlavour Strawberry) ; waitForAnyKey())
      ("Restock_flavour (truck1, Vanilla, 5)", fun () -> eventStore.Evolve truck1 (Behaviour.restock Vanilla 5))
      ("Restock_flavour (truck2, Vanilla, 5)", fun () -> eventStore.Evolve truck2 (Behaviour.restock Vanilla 5))
      ("Restock_flavour (truck1, Strawberry, 5)", fun () -> eventStore.Evolve truck1 (Behaviour.restock Strawberry 5))
      ("Restock_flavour (truck2, Strawberry, 5)", fun () -> eventStore.Evolve truck2 (Behaviour.restock Strawberry 5))
    ], ignore

  let queries =
    [
      ("FlavourInStockOfTruck (truck1, Vanilla)", fun () -> eventStore.GetStream truck1 |> project flavoursInStock |> printStockOf Vanilla ; waitForAnyKey())
      ("FlavourInStockOfTruck (truck2, Vanilla)", fun () -> eventStore.GetStream truck2 |> project flavoursInStock |> printStockOf Vanilla ; waitForAnyKey())
      ("FlavourInStockOfTruck (truck1, Strawberry)", fun () -> eventStore.GetStream truck1 |> project flavoursInStock |> printStockOf Strawberry ; waitForAnyKey())
      ("FlavourInStockOfTruck (truck2, Strawberry)", fun () -> eventStore.GetStream truck2 |> project flavoursInStock |> printStockOf Strawberry ; waitForAnyKey())
      ("FlavoursSoldOfTruck (truck1, Vanilla)", fun () -> eventStore.GetStream truck1 |> project soldFlavours |> printStockOf Vanilla ; waitForAnyKey())
      ("FlavoursSoldOfTruck (truck2, Vanilla)", fun () -> eventStore.GetStream truck2 |> project soldFlavours |> printStockOf Vanilla ; waitForAnyKey())
      ("FlavoursSoldOfTruck (truck1, Strawberry)", fun () -> eventStore.GetStream truck1 |> project soldFlavours |> printStockOf Strawberry ; waitForAnyKey())
      ("FlavoursSoldOfTruck (truck2, Strawberry)", fun () -> eventStore.GetStream truck2 |> project soldFlavours |> printStockOf Strawberry ; waitForAnyKey())
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
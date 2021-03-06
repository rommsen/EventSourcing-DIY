open Step9.Domain
open Step9.Tests
open Step9.Infrastructure
open Step9.Application
open Helper
open API
open Expecto

let printEvents header events =
  match events with
  | Ok events ->
      events
      |> List.length
      |> printfn "History for %s (Length: %i)" header

      events |> printUl

  | Error error -> printfn "Could not printEvents: %A" error

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
      ("Total History", app.GetAllEvents >> printEvents "all" >> waitForAnyKey)
      ("History Truck 1", fun () -> truck1_guid |> app.GetStream |> printEvents "Truck 1" |>  waitForAnyKey)
      ("History Truck 2", fun () -> truck2_guid |> app.GetStream |> printEvents "Truck 2" |>  waitForAnyKey)
      ("Query.FlavoursInStock (truck1, Vanilla)", fun () -> FlavourInStockOfTruck (truck1, Vanilla) |> app.HandleQuery |> runAsync |> printfn "Stock Truck 1 Vanilla: %A" |> waitForAnyKey)
      ("Query.FlavoursInStock (truck2, Vanilla)", fun () -> FlavourInStockOfTruck (truck2, Vanilla) |> app.HandleQuery |> runAsync |> printfn "Stock Truck 2 Vanilla %A" |> waitForAnyKey)
      ("Query.FlavoursInStock (truck1, Strawberry)", fun () -> FlavourInStockOfTruck (truck1, Strawberry) |> app.HandleQuery |> runAsync |> printfn "Stock Truck 1 Strawberry: %A" |> waitForAnyKey)
      ("Query.FlavoursInStock (truck2, Strawberry)", fun () -> FlavourInStockOfTruck (truck2, Strawberry) |> app.HandleQuery |> runAsync |> printfn "Stock Truck 2 Strawberry %A" |> waitForAnyKey)
      ("Sell_flavour (truck1, Vanilla)", fun () -> Sell_flavour (truck1, Vanilla) |> app.HandleCommand truck1_guid)
      ("Sell_flavour (truck2, Vanilla)", fun () -> Sell_flavour (truck2, Vanilla) |> app.HandleCommand truck2_guid)
      ("Sell_flavour (truck1, Strawberry)", fun () -> Sell_flavour (truck1, Strawberry) |> app.HandleCommand truck1_guid)
      ("Sell_flavour (truck2, Strawberry)", fun () -> Sell_flavour (truck2, Strawberry) |> app.HandleCommand truck2_guid)
      ("Restock_flavour (truck1, Vanilla, 5)", fun () -> Restock_flavour (truck1, Vanilla, 5) |> app.HandleCommand truck1_guid)
      ("Restock_flavour (truck2, Vanilla, 5)", fun () -> Restock_flavour (truck2, Vanilla, 5) |> app.HandleCommand truck2_guid)
      ("Restock_flavour (truck1, Strawberry, 5)", fun () -> Restock_flavour (truck1, Strawberry, 5) |> app.HandleCommand truck1_guid)
      ("Restock_flavour (truck2, Strawberry, 5)", fun () -> Restock_flavour (truck2, Strawberry, 5) |> app.HandleCommand truck2_guid)
    ], ignore

  main
  |> UI.Menu.initialize () "Event Sourcing DIY"
  |> ignore

  0
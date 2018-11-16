open UI
open UI.Helper
open Expecto


let step1 =
  [
    ("append [Flavour_sold Vanilla]", Step1.Program.Append_Flavour_sold_Vanilla)
    ("append [Flavour_sold Strawberry]", Step1.Program.Append_Flavour_sold_Strawberry)
    ("append [Flavour_sold Strawberry ; Flavour_went_out_of_stock Strawberry]", Step1.Program.Append_Flavour_sold_StrawberryFlavourEmptyStrawberry)
    ("Sold flavours", Step1.Program.listOfSoldFlavours >> printMapWithHeader "\nSold flavours" >> waitForAnyKey)
  ],  Step1.Program.getEvents >> printEvents


let step2 =
  [
    // ("Demo Data", Step2.Program.demoData)
    ("SellFlavour Vanilla", Step2.Program.sellFlavour Step2.Domain.Vanilla)
    ("SellFlavour Strawberry", Step2.Program.sellFlavour Step2.Domain.Strawberry)
    ("Restock Vanilla with 5 portions", Step2.Program.restock Step2.Domain.Vanilla 5)
    ("Restock Strawberry with 5 portions", Step2.Program.restock Step2.Domain.Strawberry 5)
    ("Stock of Vanilla", Step2.Program.stockOf Step2.Domain.Vanilla >> printfn "\nStock of Vanilla: %i\n" >> waitForAnyKey)
    ("Stock of Strawberry", Step2.Program.stockOf Step2.Domain.Strawberry >> printfn "\nStock of Strawberry: %i\n" >> waitForAnyKey)
  ],  Step2.Program.getEvents >> printEvents

let step3 =
  [
    // ("Demo Data", Step3.Program.demoData)
    ("SellFlavour Vanilla", Step3.Program.sellFlavour Step3.Domain.Vanilla)
    ("SellFlavour Strawberry", Step3.Program.sellFlavour Step3.Domain.Strawberry)
    ("Restock Vanilla with 5 portions", Step3.Program.restock Step3.Domain.Vanilla 5)
    ("Restock Strawberry with 5 portions", Step3.Program.restock Step3.Domain.Strawberry 5)
    ("Stock of Vanilla", Step3.Program.stockOf Step3.Domain.Vanilla >> printfn "\nStock of Vanilla: %i\n" >> waitForAnyKey)
    ("Stock of Strawberry", Step3.Program.stockOf Step3.Domain.Strawberry >> printfn "\nStock of Strawberry: %i\n" >> waitForAnyKey)
    ("Run Tests (before)", fun _ -> runTests defaultConfig Step2.Tests.tests |> ignore ; waitForAnyKey())
    ("Run Tests (after)", fun _ -> runTests defaultConfig Step3.Tests.tests |> ignore ; waitForAnyKey())
  ],  Step3.Program.getEvents >> printEvents

let step4b truck =
  [
    ("SellFlavour Vanilla", Step4.Program.sellFlavour truck Step4.Domain.Vanilla)
    ("SellFlavour Strawberry", Step4.Program.sellFlavour truck Step4.Domain.Strawberry)
    ("Restock Vanilla with 5 portions", Step4.Program.restock truck Step4.Domain.Vanilla 5)
    ("Restock Strawberry with 5 portions", Step4.Program.restock truck Step4.Domain.Strawberry 5)
    ("Stock of Vanilla", Step4.Program.stockOf truck Step4.Domain.Vanilla >> printfn "\nStock of Vanilla: %i\n" >> waitForAnyKey)
    ("Stock of Strawberry", Step4.Program.stockOf truck Step4.Domain.Strawberry >> printfn "\nStock of Strawberry: %i\n" >> waitForAnyKey)
  ],  Step4.Program.getEventStream truck >> printEvents


let step4 =
  [
     ("Demo Data", Step4.Program.demoData)
     ("Truck1", fun mailbox -> Menu.initialize mailbox "Step4 - Truck1" (step4b Step4.Program.truck1))
     ("Truck2", fun mailbox -> Menu.initialize mailbox "Step4 - Truck2" (step4b Step4.Program.truck2))
     ("Events per Aggregate", Step4.Program.getEvents >> printEventsPerAggregate >> waitForAnyKey)
     ("Run Tests", fun _ -> runTests defaultConfig Step4.Tests.tests |> ignore ; waitForAnyKey())
  ],  Step4.Program.getEvents >> printTotalHistory

[<EntryPoint>]
let main _ =
  let main =
    [
      ("Step 1", fun () -> Menu.initialize (Step1.Program.mailbox()) "Step1" step1)
      ("Step 2", fun () -> Menu.initialize (Step2.Program.mailbox()) "Step2" step2)
      ("Step 3", fun () -> Menu.initialize (Step3.Program.mailbox()) "Step3" step3)
      ("Step 4", fun () -> Menu.initialize (Step4.Program.mailbox()) "Step4" step4)
    ], ignore

  main
  |> Menu.initialize () "Event Sourcing DIY"
  |> ignore

  0
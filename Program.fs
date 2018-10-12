open UI
open UI.Helper
open Expecto


let step1 =
  [
    ("append [Flavour_sold Vanilla]", Step1.Program.Append_Flavour_sold_Vanilla)
    ("append [Flavour_sold Strawberry]", Step1.Program.Append_Flavour_sold_Strawberry)
    ("append [Flavour_sold Strawberry ; Flavour_Empty Strawberry]", Step1.Program.Append_Flavour_sold_StrawberryFlavourEmptyStrawberry)
    ("Sold flavours", Step1.Program.listOfSoldFlavours >> printMapWithHeader "\nSold flavours" >> waitForAnyKey)
  ],  Step1.Program.getEvents >> printEvents

let step2 =
  [
    ("SellFlavour Vanilla", Step2.Program.sellFlavour Step2.Domain.Vanilla)
    ("SellFlavour Strawberry", Step2.Program.sellFlavour Step2.Domain.Strawberry)
    ("Sold flavours", Step2.Program.listOfSoldFlavours >> printMapWithHeader "\nSold flavours" >> waitForAnyKey)
  ],  Step2.Program.getEvents >> printEvents

let step3 =
  [
    ("Demo Data", Step3.Program.demoData)
    ("SellFlavour Vanilla", Step3.Program.sellFlavour Step3.Domain.Vanilla)
    ("SellFlavour Strawberry", Step3.Program.sellFlavour Step3.Domain.Strawberry)
    ("Sold flavours", Step3.Program.listOfSoldFlavours >> printMapWithHeader "\nSold flavours" >> waitForAnyKey)
  ],  Step3.Program.getEvents >> printEvents

let step4 =
  [
    ("Demo Data", Step4.Program.demoData)
    ("SellFlavour Vanilla", Step4.Program.sellFlavour Step4.Domain.Vanilla)
    ("SellFlavour Strawberry", Step4.Program.sellFlavour Step4.Domain.Strawberry)
    ("Restock Vanilla with 5 portions", Step4.Program.restock Step4.Domain.Vanilla 5)
    ("Restock Strawberry with 5 portions", Step4.Program.restock Step4.Domain.Strawberry 5)
    ("Stock of Vanilla", Step4.Program.stockOf Step4.Domain.Vanilla >> printfn "\nStock of Vanilla: %i\n" >> waitForAnyKey)
    ("Stock of Strawberry", Step4.Program.stockOf Step4.Domain.Strawberry >> printfn "\nStock of Strawberry: %i\n" >> waitForAnyKey)
  ],  Step4.Program.getEvents >> printEvents

let step5 =
  [
    ("Demo Data", Step5.Program.demoData)
    ("SellFlavour Vanilla", Step5.Program.sellFlavour Step5.Domain.Vanilla)
    ("SellFlavour Strawberry", Step5.Program.sellFlavour Step5.Domain.Strawberry)
    ("Restock Vanilla with 5 portions", Step5.Program.restock Step5.Domain.Vanilla 5)
    ("Restock Strawberry with 5 portions", Step5.Program.restock Step5.Domain.Strawberry 5)
    ("Stock of Vanilla", Step5.Program.stockOf Step5.Domain.Vanilla >> printfn "\nStock of Vanilla: %i\n" >> waitForAnyKey)
    ("Stock of Strawberry", Step5.Program.stockOf Step5.Domain.Strawberry >> printfn "\nStock of Strawberry: %i\n" >> waitForAnyKey)
    ("Run Tests (before)", fun _ -> runTests defaultConfig Step4.Tests.tests |> ignore ; waitForAnyKey())
    ("Run Tests (after)", fun _ -> runTests defaultConfig Step5.Tests.tests |> ignore ; waitForAnyKey())
  ],  Step5.Program.getEvents >> printEvents

let step6b truck =
  [
    ("SellFlavour Vanilla", Step6.Program.sellFlavour truck Step6.Domain.Vanilla)
    ("SellFlavour Strawberry", Step6.Program.sellFlavour truck Step6.Domain.Strawberry)
    ("Restock Vanilla with 5 portions", Step6.Program.restock truck Step6.Domain.Vanilla 5)
    ("Restock Strawberry with 5 portions", Step6.Program.restock truck Step6.Domain.Strawberry 5)
    ("Stock of Vanilla", Step6.Program.stockOf truck Step6.Domain.Vanilla >> printfn "\nStock of Vanilla: %i\n" >> waitForAnyKey)
    ("Stock of Strawberry", Step6.Program.stockOf truck Step6.Domain.Strawberry >> printfn "\nStock of Strawberry: %i\n" >> waitForAnyKey)
  ],  Step6.Program.getEventStream truck >> printEvents


let step6 =
  [
     ("Demo Data", Step6.Program.demoData)
     ("Truck1", fun mailbox -> Menu.initialize mailbox "Step6 - Truck1" (step6b Step6.Program.truck1))
     ("Truck2", fun mailbox -> Menu.initialize mailbox "Step6 - Truck2" (step6b Step6.Program.truck2))
     ("Events per Aggregate", Step6.Program.getEvents >> printEventsPerAggregate >> waitForAnyKey)
     ("Run Tests", fun _ -> runTests defaultConfig Step6.Tests.tests |> ignore ; waitForAnyKey())
  ],  Step6.Program.getEvents >> printTotalHistory

[<EntryPoint>]
let main _ =
  let main =
    [
      ("Step 1", fun () -> Menu.initialize (Step1.Program.mailbox()) "Step1" step1)
      ("Step 2", fun () -> Menu.initialize (Step1.Program.mailbox()) "Step1" step1)
      ("Step 3", fun () -> Menu.initialize (Step2.Program.mailbox()) "Step2" step2)
      ("Step 4", fun () -> Menu.initialize (Step3.Program.mailbox()) "Step3" step3)
      ("Step 5", fun () -> Menu.initialize (Step4.Program.mailbox()) "Step4" step4)
      ("Step 6", fun () -> Menu.initialize (Step5.Program.mailbox()) "Step5" step5)
      // ("Step 7", fun () -> Menu.initialize (Step6.Program.mailbox()) "Step6" step6)
    ], ignore

  main
  |> Menu.initialize () "Event Sourcing DIY"
  |> ignore

  0
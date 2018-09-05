open UI
open UI.Helper
open System

let step1 =
  [
    ("append [IcecreamSold Vanilla]", Step1.Program.appendIcecreamSoldVanilla)
    ("append [IcecreamSold Strawberry]", Step1.Program.appendIcecreamSoldStrawberry)
    ("append [IcecreamSoldStrawberry ; Flavour_Empty Strawberry]", Step1.Program.appendIcecreamSoldStrawberryFlavourEmptyStrawberry)
  ],  Step1.Program.getEvents >> printEvents

let step2 =
  [
    ("append [IcecreamSold Vanilla]", Step2.Program.appendIcecreamSoldVanilla)
    ("append [IcecreamSold Strawberry]", Step2.Program.appendIcecreamSoldStrawberry)
    ("append [IcecreamSoldStrawberry ; Flavour_Empty Strawberry]", Step2.Program.appendIcecreamSoldStrawberryFlavourEmptyStrawberry)
    ("list of sold flavours", Step2.Program.listOfSoldFlavours >> printUlWithHeader "List of sold Flavours" >> waitForAnyKey)
  ],  Step2.Program.getEvents >> printEvents

let step3 =
  [
    ("Demo Data", Step3.Program.demoData)
    ("SellIcecream Vanilla", Step3.Program.sellIcecream Step3.Domain.Vanilla)
    ("SellIcecream Strawberry", Step3.Program.sellIcecream Step3.Domain.Strawberry)
    ("list of sold flavours", Step3.Program.listOfSoldFlavours >> printUlWithHeader "List of sold Flavours" >> waitForAnyKey)
  ],  Step3.Program.getEvents >> printEvents

let step4 =
  [
    ("Demo Data", Step4.Program.demoData)
    ("SellIcecream Vanilla", Step4.Program.sellIcecream Step4.Domain.Vanilla)
    ("SellIcecream Strawberry", Step4.Program.sellIcecream Step4.Domain.Strawberry)
    ("list of sold flavours", Step4.Program.listOfSoldFlavours >> printUlWithHeader "List of sold Flavours" >> waitForAnyKey)
  ],  Step4.Program.getEvents >> printEvents

let step5 =
  [
    ("Demo Data", Step5.Program.demoData)
    ("SellIcecream Vanilla", Step5.Program.sellIcecream Step5.Domain.Vanilla)
    ("SellIcecream Strawberry", Step5.Program.sellIcecream Step5.Domain.Strawberry)
    ("Restock Vanilla with 5 portions", Step5.Program.restock Step5.Domain.Vanilla 5)
    ("Restock Strawberry with 5 portions", Step5.Program.restock Step5.Domain.Strawberry 5)
    ("Stock of Vanilla", Step5.Program.stockOf Step5.Domain.Vanilla >> printfn "\nStock of Vanilla: %i\n" >> waitForAnyKey)
    ("Stock of Strawberry", Step5.Program.stockOf Step5.Domain.Strawberry >> printfn "\Stock of Strawberry: %i\n" >> waitForAnyKey)
  ],  Step5.Program.getEvents >> printEvents

let step6b truck =
  [
    ("SellIcecream Vanilla", Step6.Program.sellIcecream truck Step6.Domain.Vanilla)
    ("SellIcecream Strawberry", Step6.Program.sellIcecream truck Step6.Domain.Strawberry)
    ("Restock Vanilla with 5 portions", Step6.Program.restock truck Step6.Domain.Vanilla 5)
    ("Restock Strawberry with 5 portions", Step6.Program.restock truck Step6.Domain.Strawberry 5)
    ("Stock of Vanilla", Step6.Program.stockOf truck Step6.Domain.Vanilla >> printfn "\Stock of Vanilla: %i\n" >> waitForAnyKey)
    ("Stock of Strawberry", Step6.Program.stockOf truck Step6.Domain.Strawberry >> printfn "\Stock of Strawberry: %i\n" >> waitForAnyKey)
  ],  Step6.Program.getEventStream truck >> printEvents


let step6 =
  [
     ("Demo Data", Step6.Program.demoData)
     ("Truck1", fun mailbox -> Menu.initialize mailbox "Step6 - Truck1" (step6b Step6.Program.truck1))
     ("Truck2", fun mailbox -> Menu.initialize mailbox "Step6 - Truck2" (step6b Step6.Program.truck2))
  ],  Step6.Program.getEvents >> printEventsPerAggregate


[<EntryPoint>]
let main _ =

  let main =
    [
      ("Step 1", fun () -> Menu.initialize (Step1.Program.mailbox()) "Step1" step1)
      ("Step 2", fun () -> Menu.initialize (Step2.Program.mailbox()) "Step2" step2)
      ("Step 3", fun () -> Menu.initialize (Step3.Program.mailbox()) "Step3" step3)
      ("Step 4", fun () -> Menu.initialize (Step4.Program.mailbox()) "Step4" step4)
      ("Step 5", fun () -> Menu.initialize (Step5.Program.mailbox()) "Step5" step5)
      ("Step 6", fun () -> Menu.initialize (Step6.Program.mailbox()) "Step6" step6)
    ], ignore

  main
  |> Menu.initialize () "Event Sourcing DIY"
  |> ignore

  0
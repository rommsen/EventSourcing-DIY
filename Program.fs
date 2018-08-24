open System
open ConsoleMenu


let printSimpleEvents events =
  events
  |> List.length
  |> printfn "History (Length: %i)"

  events
  |> List.iteri (fun i event -> printfn " %i: %A" (i+1) event)

let step1 =
  [
    ("append [IcecreamSold Vanilla]", Step1.Program.appendIcecreamSoldVanilla)
    ("append [IcecreamSold Strawberry]", Step1.Program.appendIcecreamSoldStrawberry)
    ("append [IcecreamSoldStrawberry ; Flavour_Empty Strawberry]", Step1.Program.appendIcecreamSoldStrawberryFlavourEmptyStrawberry)
  ],  Step1.Program.getEvents >> printSimpleEvents


[<EntryPoint>]
let main argv =

  let main =
    [
      ("Step 1", fun () -> Menu.enterMenuLoop (Step1.Program.mailbox()) "Step1" step1)
    ], ignore

  main
  |> Menu.enterMenuLoop () "Event Sourcing DIY"
  |> ignore

  0
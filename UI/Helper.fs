namespace UI

module Helper =
  open System
  
  let inline printUl list =
    list
    |> List.iteri (fun i item -> printfn " %i: %A" (i+1) item)

  let inline printUlWithHeader header list =
    header |> printfn "%s:\n"
    list |> printUl

  let printEvents  events =
    events
    |> List.length
    |> printfn "History (Length: %i)"

    events |> printUl

  let printEventsPerAggregate history =
    history
    |> Map.toList
    |> List.iteri (
          fun index (truck, events) ->
            printfn "%i. Truck: %A (Length: %i)" (index+1) truck (List.length events)
            events |> printUl
            printfn ""
            )

  let waitForAnyKey () =
    Console.ReadKey() |> ignore
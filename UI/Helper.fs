namespace UI

module Helper =
  open System

  let printTotalHistory history =
    history
    |> Map.fold (fun length _ events -> length + (events |> List.length)) 0
    |> printfn "Total History Length: %i"

  let inline printUl list =
    list
    |> List.iteri (fun i item -> printfn " %i: %A" (i+1) item)

  let inline printMapWithHeader header map =
    header |> printfn "%s:\n"
    map
    |> Map.toList
    |> List.map (fun (key,value) -> sprintf "%A: %A" key value)
    |> printUl


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
            printfn "\n%i. Truck History: %A (Length: %i)" (index+1) truck (List.length events)
            events |> printUl
            printfn ""
            )

  let waitForAnyKey () =
    Console.ReadKey() |> ignore
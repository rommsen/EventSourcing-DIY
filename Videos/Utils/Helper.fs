module Helper

open System

let inline printError message details =
  Console.ForegroundColor <- ConsoleColor.Red
  printfn "\n%s" message
  Console.ResetColor()
  printfn "%A" details

let printUl list =
  list
  |> List.iteri (fun i item -> printfn " %i: %A" (i+1) item)

let runAsync asnc =
  asnc |> Async.RunSynchronously

let waitForAnyKey () =
  Console.ReadKey() |> ignore

let printEvents header events =
  events
  |> List.length
  |> printfn "History for %s (Length: %i)" header

  events |> printUl

let printTotalHistory history =
  history
  |> Map.fold (fun length _ events -> length + (events |> List.length)) 0
  |> printfn "Total History Length: %i"

let printQueryResults header result =
  result
  |> runAsync
  |> function
    | Infrastructure.QueryResult.Handled result ->
        printfn "\n%s: %A" header result

    | Infrastructure.QueryResult.NotHandled ->
        printfn "\n%s: NOT HANDLED" header

    | Infrastructure.QueryResult.QueryError error ->
        printError (sprintf "Query Error: %s" error) ""

  waitForAnyKey()
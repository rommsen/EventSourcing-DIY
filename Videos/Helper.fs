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
namespace UI

module Menu =
  open System

  let printHeader name =
    printfn "********** %s **********" name

  let printFooter<'actionParam> footerAction (actionParam : 'actionParam)  =
    printfn ""
    footerAction actionParam
    printfn ""

  let printMenu selected executionTable =
    executionTable
    |> Seq.iteri (fun index (name:string,_) ->
        if index = selected then
          Console.ForegroundColor <- ConsoleColor.Yellow
          Console.BackgroundColor <- ConsoleColor.DarkBlue

        Console.WriteLine (sprintf " %i. %s" (index + 1) name)
        Console.ResetColor())

  let initialize<'actionParam> (actionParam : 'actionParam) menu (options,footerAction) =
    Console.CursorVisible <- false

    let options =
      options @ [("Exit", ignore)]

    let rec loop selected =
      System.Console.Clear()
      printHeader menu
      printFooter footerAction actionParam
      printMenu selected options

      let userInput = System.Console.ReadKey()
      match userInput.Key with
      | ConsoleKey.DownArrow ->
          let selected =
            if selected = (options |> Seq.length) - 1 then
              0
            else
              selected + 1

          loop selected

      | ConsoleKey.UpArrow ->
          let selected =
            if selected = 0 then
              (options |> Seq.length) - 1
            else
              selected - 1

          loop selected

      | ConsoleKey.Escape ->
          ()

      | ConsoleKey.Enter ->
          if selected <> (options |> Seq.length) - 1 then
            options
            |> Seq.iteri (fun index (_,action) ->
                if index = selected
                then action actionParam)

            loop selected
          else
            ()

      | _ ->
          loop selected

    loop 0
module Step4.Domain

type Flavour =
  | Vanilla
  | Strawberry

type Event =
  | IcecreamSold of Flavour
  | Icecream_Restocked of Flavour * int
  | Flavour_empty of Flavour
  | Flavour_was_not_in_stock of Flavour


module Projections =

  open Step4.Infrastructure

  let private updateSoldIcecreams state event =
    match event with
    | IcecreamSold flavour ->
        flavour :: state

    | _ ->
        state

  let soldIcecreams : Projection<Flavour list, Event> =
    {
      Init = []
      Update = updateSoldIcecreams
    }


  let private updateIcecreamsInStock stock event =
    match event with
    | IcecreamSold flavour ->
        stock
        |> Map.tryFind flavour
        |> Option.map (fun portions -> stock |> Map.add flavour (portions - 1))
        |> Option.defaultValue stock

    | _ ->
        stock


  let icecreamsInStock : Projection<Map<Flavour, int>, Event> =
    {
      Init = Map.empty
      Update = updateIcecreamsInStock
    }


module Behaviour =

  let private numberOfFlavourInStock flavour stock =
    stock
    |> Map.tryFind flavour
    |> Option.defaultValue 0

  let sellIceCream flavour events =
    let stock =
      events
      |> List.fold Projections.icecreamsInStock.Update Projections.icecreamsInStock.Init
      |> numberOfFlavourInStock flavour

    match stock with
    | 0 -> [Flavour_was_not_in_stock flavour]
    | 1 -> [IcecreamSold flavour ; Flavour_empty flavour]
    | _ -> [IcecreamSold flavour]
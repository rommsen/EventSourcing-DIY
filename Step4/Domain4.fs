namespace Step4.Domain

type Flavour =
  | Vanilla
  | Strawberry

type Event =
  | Flavour_sold of Flavour
  | Flavour_restocked of Flavour * int
  | Flavour_empty of Flavour
  | Flavour_was_not_in_stock of Flavour


module Projections =

  open Step4.Infrastructure

  let private updateSoldIcecreams state event =
    match event with
    | Flavour_sold flavour ->
        flavour :: state

    | _ ->
        state

  let soldFlavours : Projection<Flavour list, Event> =
    {
      Init = []
      Update = updateSoldIcecreams
    }


  let private updateFlavoursInStock stock event =
    match event with
    | Flavour_sold flavour ->
        stock
        |> Map.tryFind flavour
        |> Option.map (fun portions -> stock |> Map.add flavour (portions - 1))
        |> Option.defaultValue stock

    | _ ->
        stock


  let flavoursInStock : Projection<Map<Flavour, int>, Event> =
    {
      Init = Map.empty
      Update = updateFlavoursInStock
    }


module Behaviour =

  let private stockOf flavour stock =
    stock
    |> Map.tryFind flavour
    |> Option.defaultValue 0

  let sellFlavour flavour events =
    let stock =
      events
      |> List.fold Projections.flavoursInStock.Update Projections.flavoursInStock.Init
      |> stockOf flavour

    match stock with
    | 0 -> [Flavour_was_not_in_stock flavour]
    | 1 -> [Flavour_sold flavour ; Flavour_empty flavour]
    | _ -> [Flavour_sold flavour]
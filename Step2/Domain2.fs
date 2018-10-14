namespace Step2.Domain

type Flavour =
  | Vanilla
  | Strawberry

type Event =
  | Flavour_sold of Flavour
  | Flavour_restocked of Flavour * int
  | Flavour_went_out_of_stock of Flavour
  | Flavour_was_not_in_stock of Flavour


module Projections =

  open Step2.Infrastructure

  let private updateSoldFlavours state event =
    match event with
    | Flavour_sold flavour ->
        state
        |> Map.tryFind flavour
        |> Option.defaultValue 0
        |> fun portions -> state |> Map.add flavour (portions + 1)

    | _ ->
        state

  let soldFlavours : Projection<Map<Flavour,int>, Event> =
    {
      Init = Map.empty
      Update = updateSoldFlavours
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
    | 1 -> [Flavour_sold flavour ; Flavour_went_out_of_stock flavour]
    | _ -> [Flavour_sold flavour]
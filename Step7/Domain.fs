module Step7.Domain


type Truck = System.Guid

type Flavour =
  | Vanilla
  | Strawberry

type Event =
  | Flavour_sold of Flavour
  | Icecream_Restocked of Flavour * int
  | Flavour_empty of Flavour
  | Flavour_was_not_in_stock of Flavour


module Projections =

  open Step7.Infrastructure

  let project projection events =
    events |> List.fold projection.Update projection.Init

  let private updateSoldIcecreams state event =
    match event with
    | Flavour_sold flavour ->
        flavour :: state

    | _ ->
        state

  let soldIcecreams : Projection<Flavour list, Event> =
    {
      Init = []
      Update = updateSoldIcecreams
    }

  let restock flavour number stock =
    stock
    |> Map.tryFind flavour
    |> Option.defaultValue 0
    |> fun portions -> stock |> Map.add flavour (portions + number)

  let updateIcecreamsInStock stock event =
    match event with
    | Flavour_sold flavour ->
        stock |> restock flavour -1

    | Icecream_Restocked (flavour, portions) ->
        stock |> restock flavour portions

    | _ ->
        stock


  let icecreamsInStock : Projection<Map<Flavour, int>, Event> =
    {
      Init = Map.empty
      Update = updateIcecreamsInStock
    }

  let stockOf flavour stock =
    stock
    |> Map.tryFind flavour
    |> Option.defaultValue 0


module Behaviour =

  open Projections

  let sellIceCream flavour events =
    let stock =
      events
      |> project icecreamsInStock
      |> stockOf flavour

    match stock with
    | 0 -> [Flavour_was_not_in_stock flavour]
    | 1 -> [Flavour_sold flavour ; Flavour_empty flavour]
    | _ -> [Flavour_sold flavour]


  let restock flavour portions events =
    [ Icecream_Restocked (flavour,portions) ]
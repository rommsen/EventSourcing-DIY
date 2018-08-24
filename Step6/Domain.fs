module Step6.Domain


type Truck = System.Guid

type Flavour =
  | Vanilla
  | Strawberry

type Event =
  | IcecreamSold of Flavour
  | Icecream_Restocked of Flavour * int
  | Flavour_empty of Flavour
  | Flavour_was_not_in_stock of Flavour


module Projections =

  open Step6.Infrastructure

  let project projection events =
    events |> List.fold projection.Update projection.Init

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

  let restock flavour number  stock =
    stock
    |> Map.tryFind flavour
    |> Option.map (fun portions -> stock |> Map.add flavour (portions + number))
    |> Option.defaultValue stock

  let updateIcecreamsInStock stock event =
    match event with
    | IcecreamSold flavour ->
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


module Behaviour =

  open Projections

  let private numberOfFlavourInStock flavour stock =
    stock
    |> Map.tryFind flavour
    |> Option.defaultValue 0

  let sellIceCream flavour events =
    let stock =
      events
      |> project icecreamsInStock
      |> numberOfFlavourInStock flavour

    match stock with
    | 0 -> [Flavour_was_not_in_stock flavour]
    | 1 -> [IcecreamSold flavour ; Flavour_empty flavour]
    | _ -> [IcecreamSold flavour]


  let restock flavour portions events =
    [ Icecream_Restocked (flavour,portions) ]
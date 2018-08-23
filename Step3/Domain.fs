module Step3.Domain

type Flavour =
  | Vanilla
  | Strawberry

type Event =
  | IcecreamSold of Flavour
  | Icecream_Restocked of Flavour * int
  | Flavour_empty of Flavour
  | Flavour_was_not_in_stock of Flavour


module Projections =

  open Step3.Infrastructure

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

module Behaviour =

  let sellIceCream flavour events =
    [IcecreamSold flavour]
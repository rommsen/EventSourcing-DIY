namespace Step3.Domain

type Flavour =
  | Vanilla
  | Strawberry

type Event =
  | Flavour_sold of Flavour
  | Flavour_restocked of Flavour * int
  | Flavour_empty of Flavour
  | Flavour_was_not_in_stock of Flavour


module Projections =

  open Step3.Infrastructure

  let private updateSoldFlavours state event =
    match event with
    | Flavour_sold flavour ->
        flavour :: state

    | _ ->
        state

  let soldFlavours : Projection<Flavour list, Event> =
    {
      Init = []
      Update = updateSoldFlavours
    }

module Behaviour =

  let sellFlavour flavour events =
    [Flavour_sold flavour]
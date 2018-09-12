namespace Step1.Domain

type Flavour =
  | Vanilla
  | Strawberry

type Event =
  | Flavour_sold of Flavour
  | Flavour_restocked of Flavour * int
  | Flavour_empty of Flavour
  | Flavour_was_not_in_stock of Flavour
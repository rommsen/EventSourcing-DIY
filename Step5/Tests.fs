module Step5.Tests


open Expecto
open Expecto.Expect
open Step5.Domain

let Given = id

let When handler events =
  handler events

let Then expectedEvents events =
  equal events expectedEvents "Events should equal expected events"


let tests =
  testList "sellIceCream"
    [
      test "Flavour_sold" {
        Given
          [
              Flavour_restocked (Vanilla,5)
              Flavour_sold Vanilla
              Flavour_sold Vanilla
          ]
        |> When (Behaviour.sellIceCream Vanilla)
        |> Then [Flavour_sold Vanilla]
      }

      test "Flavour_was_not_in_stock" {
        Given
          [
              Flavour_restocked (Vanilla,5)
              Flavour_restocked (Strawberry,2)
              Flavour_sold Vanilla
              Flavour_sold Vanilla
              Flavour_sold Strawberry
              Flavour_sold Strawberry
              Flavour_empty Strawberry
          ]
        |> When (Behaviour.sellIceCream Strawberry)
        |> Then [Flavour_was_not_in_stock Strawberry]
      }

    ]




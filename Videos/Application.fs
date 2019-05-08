namespace Application

module API =

  open Domain

  type Query =
    | FlavourInStockOfTruck of Truck * Flavour
    | FlavourInStockOfAll of Flavour
    | FlavoursSoldOfTruck of Truck * Flavour
    | FlavoursSoldOfAll of Flavour



module QueryHandlers =
  open API
  open Domain
  open Infrastructure
  open Projections

  let flavours eventStore =
    let handleQuery query =
      match query with
      | FlavourInStockOfTruck(Truck truck, flavour) ->
          async {
            let state =
              eventStore.GetStream truck |> project flavoursInStock

            return
              state
              |> Map.tryFind flavour
              |> Option.defaultValue 0
              |> box
              |> Handled
          }

      | FlavoursSoldOfTruck(Truck truck, flavour) ->
          async {
            let state =
              eventStore.GetStream truck |> project soldFlavours

            return
              state
              |> Map.tryFind flavour
              |> Option.defaultValue 0
              |> box
              |> Handled
          }

       | _ ->
          async { return NotHandled }


    { Handle = handleQuery }
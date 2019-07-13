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

  let flavours (eventStore : EventStore<Event>) =
    let handleQuery query =
      match query with
      | FlavourInStockOfTruck(Truck truck, flavour) ->
          async {
            match! eventStore.GetStream truck with
            | Ok stream ->
                return
                  stream
                  |> List.map (fun envelope -> envelope.Event)
                  |> project flavoursInStock
                  |> Map.tryFind flavour
                  |> Option.defaultValue 0
                  |> box
                  |> Handled

            | Error error ->
                return QueryError error
          }

      | FlavoursSoldOfTruck(Truck truck, flavour) ->
          async {
            match! eventStore.GetStream truck with
            | Ok stream ->
                return
                  stream
                  |> List.map (fun envelope -> envelope.Event)
                  |> project soldFlavours
                  |> Map.tryFind flavour
                  |> Option.defaultValue 0
                  |> box
                  |> Handled

            | Error error ->
                return QueryError error
          }
       | _ ->
          async { return NotHandled }


    { Handle = handleQuery }
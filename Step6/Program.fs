module Step6.Program

open Step6.Domain
open Step6.Infrastructure

let eventStore : EventStore<Event> = EventStore.initialize()

let truck1 = System.Guid.NewGuid()
let truck2 = System.Guid.NewGuid()

eventStore.Evolve truck1 (Behaviour.restock Strawberry 5)
eventStore.Evolve truck1 (Behaviour.restock Vanilla 1)
eventStore.Evolve truck1 (Behaviour.sellIceCream Strawberry)
eventStore.Evolve truck1 (Behaviour.sellIceCream Vanilla)


let trucksWithSoldNumberOfIcecreams events =
  events
  |> Map.map (fun _ events ->
        events
        |> Projections.project Projections.soldIcecreams
        |> List.length)


eventStore.Get()
|> trucksWithSoldNumberOfIcecreams
|> printfn "%A"
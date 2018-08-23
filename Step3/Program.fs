module Step3.Program

open Step3.Domain
open Step3.Infrastructure

let eventStore : EventStore<Event> = EventStore.initialize()

let sold () =
  eventStore.Get()
  |> List.fold Projections.soldIcecreams.Update Projections.soldIcecreams.Init

eventStore.Append [IcecreamSold Vanilla]
eventStore.Append [IcecreamSold Strawberry ; Flavour_empty Strawberry]
eventStore.Append [IcecreamSold Vanilla]
eventStore.Append [IcecreamSold Vanilla]

sold()

eventStore.Evolve (Behaviour.sellIceCream Strawberry)
eventStore.Evolve (Behaviour.sellIceCream Vanilla)

sold()
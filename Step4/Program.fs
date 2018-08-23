module Step4.Program

open Step4.Domain
open Step4.Infrastructure

let eventStore : EventStore<Event> = EventStore.initialize()

let sold () =
  eventStore.Get()
  |> List.fold Projections.soldIcecreams.Update Projections.soldIcecreams.Init

eventStore.Evolve (Behaviour.sellIceCream Strawberry)
eventStore.Evolve (Behaviour.sellIceCream Vanilla)

sold()
module Step5.Program

open Step5.Domain
open Step5.Infrastructure

let eventStore : EventStore<Event> = EventStore.initialize()

eventStore.Evolve (Behaviour.restock Strawberry 5)
eventStore.Evolve (Behaviour.restock Vanilla 1)
eventStore.Evolve (Behaviour.sellIceCream Strawberry)
eventStore.Evolve (Behaviour.sellIceCream Vanilla)

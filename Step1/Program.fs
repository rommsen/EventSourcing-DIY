module Step1.Program

open Step1.Domain
open Step1.Infrastructure

let eventStore : EventStore<Event> = EventStore.init()

eventStore.Append [IcecreamSold Vanilla]
eventStore.Append [IcecreamSold Strawberry ; Flavour_empty Strawberry]
eventStore.Append [IcecreamSold Vanilla]
eventStore.Append [IcecreamSold Vanilla]
module Domain

open Infrastructure


  // 2 Beispiele in der Domäne

  // wie werden aus Events State

  // einfach Funktionen, kein Subscriber. Wir müssen also die Events im State halten und abfragen

  // Aktion holt sich auch den Stream und lässt die Projektion durchlaufen. Aber welchen Stream?

// brauchen möglichkeit State zu speichern, vllt ein Objekt?
// Aktor erklären
// Mailbox Prozessor erklären
// brauchen eine Antwort

// zeige Type Driven: erst später den Event Store implementieren


// Domain
type Flavour =
  | Vanilla
  | Strawberry


type Event =
  | IcecreamSold of Flavour
  | Icecream_Restocked of Flavour * int




let updateSoldIcecreams state event =
  match event with
  | IcecreamSold flavour ->
      flavour :: state

  | _ ->
      state

let soldIcecreamsProjection : Projection<Flavour list, Event> =
  {
    InitialState = []
    UpdateState = updateSoldIcecreams
  }

let restock flavour number  stock =
  stock
  |> Map.tryFind flavour
  |> Option.map (fun portions -> stock |> Map.add flavour (portions + number))
  |> Option.defaultValue stock

let updateIcecreamsInStock stock event =
  match event with
  // | IcecreamSold flavour ->
  //     stock
  //     |> Map.tryFind flavour
  //     |> Option.map (fun portions -> stock |> Map.add flavour (portions - 1))
  //     |> Option.defaultValue stock

  // | Icecream_Restocked (flavour, portions) ->
  //     stock
  //     |> Map.tryFind flavour
  //     |> Option.map (fun portions -> stock |> Map.add flavour (portions + portions))
  //     |> Option.defaultValue stock
  | IcecreamSold flavour ->
      stock |> restock flavour -1

  | Icecream_Restocked (flavour, portions) ->
       stock |> restock flavour portions

let icecreamsInStockProjection : Projection<Map<Flavour, int>, Event> =
  {
    InitialState = Map.empty
    UpdateState = updateIcecreamsInStock
  }

// brauche irgendwie mehr events und Projektionen

let soldIceCreams getEvents =
  let events =
    getEvents()

  events
  |> List.fold soldIcecreamsProjection.UpdateState soldIcecreamsProjection.InitialState


// hier dann Program
let eventStoreReal : EventStore<Event> = eventStore()

eventStoreReal.Append [IcecreamSold Vanilla]
eventStoreReal.Append [IcecreamSold Strawberry]
eventStoreReal.Append [IcecreamSold Vanilla]
eventStoreReal.Append [IcecreamSold Vanilla]


let sold = soldIceCreams eventStoreReal.Get


// ab hier dann Aktionen
let sellIceCream flavour =
  IcecreamSold flavour


// Frage: wie mit Problemen umgehen

// Erweiterung der Domäne

// wie viel in Stock
// Available, nicht available









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


// mehrere Trucks

// Domain

type Truck = System.Guid

type Flavour =
  | Vanilla
  | Strawberry



type Event =
  | IcecreamSold of Flavour
  | Icecream_Restocked of Flavour * int
  | Flavour_empty of Flavour
  | Flavour_was_not_in_stock of Flavour


let updateSoldIcecreams state event =
  match event with
  | IcecreamSold flavour ->
      flavour :: state

  | _ ->
      state

let soldIcecreamsProjection : Projection<Flavour list, Event> =
  {
    Init = []
    Update = updateSoldIcecreams
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

let flavourInStock flavour stock =
  stock
  |> Map.tryFind flavour
  |> Option.defaultValue 0



let icecreamsInStock : Projection<Map<Flavour, int>, Event> =
  {
    Init = Map.empty
    Update = updateIcecreamsInStock
  }

let poject projection events =
   events |> List.fold projection.Update projection.Init

// brauche irgendwie mehr events und Projektionen

let soldIceCreams getEvents =
  getEvents()
  |> List.fold soldIcecreamsProjection.Update soldIcecreamsProjection.Init
  // erstmal so, dann beim 2. zeigen wieder das gleich
  // eine Project Funktion bauen (baue was du brauchst)



// hier dann Program
let eventStoreReal : EventStore<Event> = eventStore()



// let sold = soldIceCreams eventStoreReal.Get


// // ab hier dann Aktionen
// let sellIceCream flavour events =
//   [IcecreamSold flavour]

let sellIceCream flavour events =
  let stock =
    events
    |> poject icecreamsInStock
    |> flavourInStock flavour

  match stock with
  | 0 -> [Flavour_was_not_in_stock flavour]
  | 1 -> [IcecreamSold flavour ; Flavour_empty flavour]
  | _ -> [IcecreamSold flavour]


// wenn empty muss neu bestellt werden

let truck = System.Guid.NewGuid()


eventStoreReal.Run truck (sellIceCream Vanilla)
eventStoreReal.Run truck (sellIceCream Strawberry)
eventStoreReal.Run truck (sellIceCream Vanilla)
eventStoreReal.Run truck (sellIceCream Vanilla)

// Frage: wie mit Problemen umgehen

// auch Events

// Erweiterung der Domäne

// wie viel in Stock
// Available, nicht available




// erst rein mit append zeigen, dann sagen, hmm das bringt nichts, keine Regeln
// dann mit Run

// erst mal nur für eins dann mit aggregaten
// nicht alle events anpassen, deswegen nehmen wir das Aggregate auf

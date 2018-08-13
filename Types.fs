module Types =

  // Domain
  type Flavour =
    | Vanilla
    | Strawberry

  type Event =
    | IcecreamSold of Flavour


  // endregion

  type Projector<'State,'Event> = 'State -> 'Event -> 'State

  // einfache Projektion
  type Projection<'State,'Event> =
    {
      InitialState : 'State
      UpdateState : Projector<'State,'Event>
    }


  type EventStore<'Event> =
    {
      Append : 'Event list -> unit
      Get :  unit -> 'Event list
    }


    // 2 Beispiele in der Domäne

    // wie werden aus Events State

    // einfach Funktionen, kein Subscriber. Wir müssen also die Events im State halten und abfragen

    // Aktion holt sich auch den Stream und lässt die Projektion durchlaufen. Aber welchen Stream?

  // brauchen möglichkeit State zu speichern, vllt ein Objekt?
  // Aktor erklären
  // Mailbox Prozessor erklären
  // brauchen eine Antwort

  // zeige Type Driven: erst später den Event Store implementieren

  // später Get für Stream
  // später: warte aufs Commit
  type Msg<'Event> =
    | Get of AsyncReplyChannel<'Event list>
    | Append of 'Event list

  let eventStore () : EventStore<'Event> =
    let events = []

    let mailbox =
      MailboxProcessor.Start(fun inbox ->
        // printfn "Start"
        let rec loop events =
            async {
              let! msg = inbox.Receive()

              match msg with
              | Append newEvents ->
                  return! loop (List.concat [ events ; newEvents ])

              | Get reply ->
                  reply.Reply events
                  return! loop events
            }

        loop events
      )

    let append events =
      mailbox.Post (Append events)

    {
      Append = append
      Get = fun () ->  mailbox.PostAndReply(Get)
    }


  let update state event =
    match event with
    | IcecreamSold flavour ->
        flavour :: state

  let soldIcecreamsProjection : Projection<Flavour list, Event> =
    {
      InitialState = []
      UpdateState = update
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


  // Erweiterung der Domäne

  // wie viel in Stock
  // Available, nicht available









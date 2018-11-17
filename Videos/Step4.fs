module Infrastructure =

  type EventProducer<'Event> =
    'Event list -> 'Event list

  type EventStore<'Event> =
    {
      Get : unit -> 'Event list
      Append : 'Event list -> unit
      Evolve : EventProducer<'Event> -> unit
    }

  type Projection<'State,'Event> =
    {
      Init : 'State
      Update : 'State -> 'Event -> 'State
    }


  module EventStore =

    type Msg<'Event> =
      | Get of AsyncReplyChannel<'Event list>
      | Append of 'Event list
      | Evolve of EventProducer<'Event>

    let initialize () : EventStore<'Event> =
      let history = []

      let mailbox =
        MailboxProcessor.Start(fun inbox ->
          let rec loop history =
            async {
              let! msg = inbox.Receive()

              match msg with
              | Get reply ->
                  reply.Reply history
                  return! loop history

              | Append events  ->
                  return! loop (history @ events)

              | Evolve producer ->
                  return! loop (history @ producer history)
            }

          loop history
        )

      let append events =
        events
        |> Append
        |> mailbox.Post

      let evolve producer =
        producer
        |> Evolve
        |> mailbox.Post

      {
        Get = fun () ->  mailbox.PostAndReply Get
        Append = append
        Evolve = evolve
      }


module Domain =

  type Flavour =
    | Vanilla
    | Strawberry

  type Event =
    | Flavour_sold of Flavour
    | Flavour_restocked of Flavour * int
    | Flavour_went_out_of_stock of Flavour
    | Flavour_was_not_in_stock of Flavour


module Projections =

  open Domain
  open Infrastructure

  let project projection events =
    events |> List.fold projection.Update projection.Init

  let private updateSoldFlavours state event =
    match event with
    | Flavour_sold flavour ->
        state
        |> Map.tryFind flavour
        |> Option.defaultValue 0
        |> fun portions -> state |> Map.add flavour (portions + 1)

    | _ ->
        state

  let soldFlavours : Projection<Map<Flavour,int>, Event> =
    {
      Init = Map.empty
      Update = updateSoldFlavours
    }

  let restock flavour number stock =
    stock
    |> Map.tryFind flavour
    |> Option.map (fun portions -> stock |> Map.add flavour (portions + number))
    |> Option.defaultValue stock

  let updateFlavoursInStock stock event =
    match event with
    | Flavour_sold flavour ->
        stock |> restock flavour -1

    | Flavour_restocked (flavour, portions) ->
        stock |> restock flavour portions

    | _ ->
        stock

  let flavoursInStock : Projection<Map<Flavour, int>, Event> =
    {
      Init = Map.empty
      Update = updateFlavoursInStock
    }

  let stockOf flavour stock =
    stock
    |> Map.tryFind flavour
    |> Option.defaultValue 0


module Behaviour =

  open Domain
  open Projections

  let sellFlavour flavour events =
    let stock =
      events
      |> project flavoursInStock
      |> stockOf flavour

    match stock with
    | 0 -> [Flavour_was_not_in_stock flavour]
    | 1 -> [Flavour_sold flavour ; Flavour_went_out_of_stock flavour]
    | _ -> [Flavour_sold flavour]


  let restock flavour portions events =
    [ Flavour_restocked (flavour,portions) ]


module Tests =

  open Expecto
  open Expecto.Expect
  open Domain

  let Given = id

  let When handler events =
    handler events

  let Then expectedEvents events =
    equal events expectedEvents "Events should equal expected events"

  let tests =
    testList "sellFlavour"
      [
        test "Flavour_sold" {
          Given
            [
              Flavour_restocked (Vanilla,5)
              Flavour_sold Vanilla
              Flavour_sold Vanilla
            ]
          |> When (Behaviour.sellFlavour Vanilla)
          |> Then [Flavour_sold Vanilla]
        }

        test "Flavour_was_not_in_stock" {
          Given
            [
              Flavour_restocked (Vanilla,5)
              Flavour_restocked (Strawberry,2)
              Flavour_sold Vanilla
              Flavour_sold Vanilla
              Flavour_sold Strawberry
              Flavour_sold Strawberry
              Flavour_went_out_of_stock Strawberry
            ]
          |> When (Behaviour.sellFlavour Strawberry)
          |> Then [Flavour_was_not_in_stock Strawberry]
        }

        test "Flavour_restocked" {
          Given []
          |> When (Behaviour.restock Vanilla 5 )
          |> Then [Flavour_restocked (Vanilla,5)]
        }
      ]



module Program =
  open Infrastructure
  open Domain
  open Projections

  // todo erstmal tests laufen lassen
  // dann fixen
  // dann tests laufen lassen

  let eventStore : EventStore<Event> = EventStore.initialize()

  eventStore.Evolve (Behaviour.sellFlavour flavour)

  eventStore.Evolve (Behaviour.restock flavour portions)

  // todo events zeigen, , jetzt sollte es richtig sein

  eventStore.Get()
  |> project flavoursInStock
  |> stockOf Vanilla
  |> printfn "%A"
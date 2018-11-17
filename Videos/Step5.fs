module Infrastructure =

  type Aggregate = System.Guid

  type EventProducer<'Event> =
    'Event list -> 'Event list

  type EventStore<'Event> =
    {
      Get : unit -> Map<Aggregate,'Event list>
      GetStream : Aggregate -> 'Event list
      Append : Aggregate -> 'Event list -> unit
      Evolve : Aggregate -> EventProducer<'Event> -> unit
    }

  type Projection<'State,'Event> =
    {
      Init : 'State
      Update : 'State -> 'Event -> 'State
    }


  module EventStore =

    type Msg<'Event> =
      | Get of AsyncReplyChannel<Map<Aggregate,'Event list>>
      | GetStream of Aggregate * AsyncReplyChannel<'Event list>
      | Append of  Aggregate * 'Event list
      | Evolve of Aggregate * EventProducer<'Event>

    let initialize () : EventStore<'Event> =
      let history : Map<Aggregate,'Event> = Map.empty

      let mailbox =
        MailboxProcessor.Start(fun inbox ->
          let rec loop history =
            async {
              let! msg = inbox.Receive()

              match msg with
              | Get reply ->
                  reply.Reply history
                  return! loop history

              | GetStream (aggregate,reply) ->
                  history
                  |> Map.tryFind aggregate
                  |> Option.defaultValue []
                  |> reply.Reply

                  return! loop history

              | Append (aggregate,events)  ->
                  let stream_history =
                    history
                    |> Map.tryFind aggregate
                    |> Option.defaultValue []

                  return! loop (
                      history
                      |> Map.add aggregate (stream_history @ events))

              | Evolve (aggregate,producer) ->
                  let stream_history =
                    history
                    |> Map.tryFind aggregate
                    |> Option.defaultValue []

                  let events =
                    stream_history
                    |> producer

                  return! loop (
                      history
                      |> Map.add aggregate (stream_history @ events)
                  )
            }

          loop history
        )

      let getStream aggregate =
        mailbox.PostAndReply (fun reply -> (aggregate,reply) |> GetStream)

      let append aggregate events =
        (aggregate,events)
        |> Append
        |> mailbox.Post

      let evolve aggregate producer =
        (aggregate,producer)
        |> Evolve
        |> mailbox.Post

      {
        Get = fun () ->  mailbox.PostAndReply Get
        GetStream = getStream
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

  let truck1 = System.Guid.NewGuid()
  let truck2 = System.Guid.NewGuid()

  let eventStore : EventStore<Event> = EventStore.initialize()

  eventStore.Evolve truck1 (Behaviour.sellFlavour flavour)

  eventStore.Evolve truck1 (Behaviour.restock flavour portions)

  // todo events zeigen, , jetzt sollte es richtig sein

  truck1
  |> eventStore.GetStream
  |> project flavoursInStock
  |> stockOf Vanilla
  |> printfn "%A"
module EventStore =

  type Aggregate = System.Guid

  type EventProducer<'Event> =
    'Event list -> 'Event list

  type EventSubscription<'Event> = 
    Aggregate -> 'Event list -> unit
  

  type EventStore<'Event> =
    {
      Get : unit -> Map<Aggregate,'Event list>
      GetStream : Aggregate -> 'Event list
      Append : Aggregate -> 'Event list -> unit
      Evolve : Aggregate -> EventProducer<'Event> -> unit
      Subscribe : EventSubscription<'Event>-> unit
    }

  type Msg<'Event> =
    | Get of AsyncReplyChannel<Map<Aggregate,'Event list>>
    | GetStream of Aggregate * AsyncReplyChannel<'Event list>
    | Append of  Aggregate * 'Event list
    | Evolve of Aggregate * EventProducer<'Event>
    | Subscribe of EventSubscription<'Event>


  let streamFor aggregate history =
    history
    |> Map.tryFind aggregate
    |> Option.defaultValue []  


  let appendFor aggregate history new_events stream_history =
    history |> Map.add aggregate (stream_history @ new_events)

  let notifySubscriber aggregate events subscriber = 
    events |> subscriber aggregate

  let notifySubscribers aggregate new_events subscriptions =
    subscriptions |> List.iter (notifySubscriber aggregate new_events)


  let initialize () : EventStore<'Event> =
    let history : Map<Aggregate,'Event> = Map.empty

    let mailbox =
      MailboxProcessor.Start(fun inbox ->
        let rec loop (history,subscriptions : EventSubscription<'Event> list) =
          async {
            let! msg = inbox.Receive()

            match msg with
            | Get reply ->
                reply.Reply history
                return! loop (history,subscriptions)

            | GetStream (aggregate,reply) ->
                history
                |> streamFor aggregate
                |> reply.Reply

                return! loop (history,subscriptions)

            | Append (aggregate,events)  ->
                let new_history =
                  history
                  |> streamFor aggregate
                  |> appendFor aggregate history events

                return! loop (new_history, subscriptions)

            | Evolve (aggregate,producer) ->
                let stream_history =
                  history |> streamFor aggregate

                let new_events =
                  stream_history |> producer

                let newHistory =
                  appendFor aggregate history new_events stream_history 

                do subscriptions |> notifySubscribers aggregate new_events           

                return! loop (newHistory, subscriptions)

            | Subscribe subscription ->
                subscription |> notify
                // Idee: gib möglichkeiten zum slicen mit

                return! loop (history, subscription :: subscriptions)

                

                // Frage wohin wir optimieren wollen?
                // Zugriffe auf Streams?
                // alle Events
                // Memory?

                // wenn alle Events dann möchte man sie schon in Order haben

                // Eine Event Subscription braucht sie auf jeden Fall in der Order

          }

        loop (history,[])
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

    let subscribe (subscription : EventSubscription<_>) =
      subscription
      |> Subscribe
      |> mailbox.Post    

    {
      Get = fun () ->  mailbox.PostAndReply Get
      GetStream = getStream
      Append = append
      Evolve = evolve
      Subscribe = subscribe
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

  type Projection<'State,'Event> =
    {
      Init : 'State
      Update : 'State -> 'Event -> 'State
    }
  let project projection events =
    events |> List.fold projection.Update projection.Init

  let soldOfFlavour flavour state =
    state
    |> Map.tryFind flavour
    |> Option.defaultValue 0  

  let private updateSoldFlavours state event =
    match event with
    | Flavour_sold flavour ->
        state
        |> soldOfFlavour flavour
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
    |> Option.defaultValue 0
    |> fun portions -> stock |> Map.add flavour (portions + number)

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


module ReadModels =
  open Projections 

  type ReadModel<'State, 'Event> =
    {
      Subscribe : 'Event list -> unit
      Get : unit -> 'State
    } 


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


module Helper =

  open Expecto
  open Projections

  let printUl list =
    list
    |> List.iteri (fun i item -> printfn " %i: %A" (i+1) item)

  let printEvents header events =
    events
    |> List.length
    |> printfn "History for %s (Length: %i)" header

    events |> printUl


  let printTotalHistory history =
    history
    |> Map.fold (fun length _ events -> length + (events |> List.length)) 0
    |> printfn "Total History Length: %i"


  let printSoldFlavour flavour state =
    state
    |> soldOfFlavour flavour
    |> printfn "Sold %A: %i" flavour

  let printStockOf flavour state =
    state
    |> stockOf flavour
    |> printfn "Stock of %A: %i" flavour


  let runTests () =
    runTests defaultConfig Tests.tests |> ignore



open EventStore
open Domain
open Projections
open Helper

[<EntryPoint>]
let main _ =

  let truck1 = System.Guid.NewGuid()
  let truck2 = System.Guid.NewGuid()

  runTests ()

  let eventStore : EventStore<Event> = EventStore.initialize()

  eventStore.Evolve truck1 (Behaviour.sellFlavour Vanilla)
  eventStore.Evolve truck1 (Behaviour.sellFlavour Strawberry)
  eventStore.Evolve truck1 (Behaviour.restock Vanilla 5)
  eventStore.Evolve truck1 (Behaviour.sellFlavour Vanilla)

  eventStore.Evolve truck2 (Behaviour.restock Strawberry 3)
  eventStore.Evolve truck2 (Behaviour.sellFlavour Strawberry)
  eventStore.Evolve truck2 (Behaviour.sellFlavour Strawberry)
  eventStore.Evolve truck2 (Behaviour.sellFlavour Strawberry)

  let events_truck_1 = eventStore.GetStream truck1
  let events_truck_2 = eventStore.GetStream truck2

  events_truck_1 |> printEvents "Truck 1"
  events_truck_2 |> printEvents "Truck 2"

  eventStore.Get()
  |> printTotalHistory

  0
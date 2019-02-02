module EventStore =

  type Stream = System.Guid

  type EventProducer<'Event> =
    'Event list -> 'Event list


  type StreamEvent<'Event> =
    {
      Stream : Stream 
      Event : 'Event
    }  

  type EventListener<'Event> = 
    StreamEvent<'Event> -> unit
  

  type EventStore<'Event> =
    {
      Get : unit -> StreamEvent<'Event> list
      GetStream : Stream -> 'Event list
      Append : Stream -> 'Event list -> unit
      Evolve : Stream -> EventProducer<'Event> -> unit
      Subscribe : EventListener<'Event>-> unit
    }

  type Msg<'Event> =
    | Get of AsyncReplyChannel<StreamEvent<'Event> list>
    | GetStream of Stream * AsyncReplyChannel<'Event list>
    | Append of  Stream * 'Event list
    | Evolve of Stream * EventProducer<'Event>
    | Subscribe of EventListener<'Event>




  let streamFor stream history =
    history
    |> List.filter (fun streamEvent -> streamEvent.Stream = stream)

    
  let asEvents streamEvents =
    streamEvents 
    |> List.map (fun streamEvent -> streamEvent.Event)


  let asStreamEvents stream events =
    events
    |> List.map (fun event -> { Stream = stream ; Event = event })  

  let appendFor stream history new_events stream_history =
    new_events 
    |> (@) stream_history

  let notifyEventListeners events subscriptions =
    subscriptions 
    |> List.iter (fun subscription -> events |> List.iter subscription)


  let initialize () : EventStore<'Event> =
    let history : StreamEvent<'Event> list = []

    let mailbox =
      MailboxProcessor.Start(fun inbox ->
        let rec loop (history,eventListeners : EventListener<'Event> list) =
          async {
            let! msg = inbox.Receive()

            match msg with
            | Get reply ->
                reply.Reply history
                return! loop (history,eventListeners)

            | GetStream (stream,reply) ->
                history
                |> streamFor stream
                |> asEvents
                |> reply.Reply

                return! loop (history,eventListeners)

            | Append (stream,events)  ->
                let new_history =
                  history
                  |> streamFor stream
                  |> appendFor stream history (events |> asStreamEvents stream)

                return! loop (new_history, eventListeners)

            | Evolve (stream,producer) ->
                let stream_history =
                  history |> streamFor stream

                let new_events =
                  stream_history 
                  |> asEvents
                  |> producer
                  |> asStreamEvents stream

                let newHistory =
                  appendFor stream history new_events stream_history 

                do eventListeners |> notifyEventListeners new_events           

                return! loop (newHistory, eventListeners)

            | Subscribe listener ->
                do history |> List.iter listener     

                return! loop (history, listener :: eventListeners)

                // Idee: gib möglichkeiten subscribern zum slicen mit

                // Frage wohin wir optimieren wollen?
                // Zugriffe auf Streams?
                // alle Events
                // Memory?
                // es h#ngt davon ab wohin man optimieren möchte

                // wenn alle Events dann möchte man sie schon in Order haben

                // Eine Event Subscription braucht sie auf jeden Fall in der Order

                // achtung Prototype: eventuell structs

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

    let subscribe (subscription : EventListener<_>) =
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


  type Truck = Truck of System.Guid  


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

module Queries =
  type QueryResult<'Result> =
    | Handled of 'Result 
    | NotHandled  

  type QueryHandler<'Query,'Result> =
    'Query -> QueryResult<'Result>

  type Msg<'Query,'Result> =
    | Query of 'Query * AsyncReplyChannel<QueryResult<'Result>>

  let rec private oneOf (queryHandler : QueryHandler<_,_> list) query =
    match queryHandler with
    | handler :: rest ->
        match handler query with
        | NotHandled ->
            oneOf rest query

        | Handled response ->
            Handled response

    | _ -> NotHandled

  let queryHandler (queryHandler : QueryHandler<_,_> list) : QueryHandler<_,_> =
    let agent =
      MailboxProcessor.Start(fun inbox ->
        let rec loop() =
          async {
            let! msg = inbox.Receive()

            match msg with
            | Query (query,reply)->
                oneOf queryHandler query
                |> reply.Reply

            return! loop()
          }

        loop()
      )

    let queryHandler query =
      agent.PostAndReply(fun reply -> Query (query,reply))

    queryHandler

module Readmodels =
  open EventStore 
  open Domain
  open Queries

  type Query =
    | Trucks 
    | FlavoursInStock of Truck * Flavour

  type ReadModel<'Event, 'Query, 'Result> =
    {
      EventListener : EventListener<'Event>
      QueryHandler : QueryHandler<'Query,'Result>
    } 

    (*

      Was erwarten wir von einem Readmodel
      - spezifische Abfragen -> Query?
      - projections für einen Stream
      - projections für alle Streams


      Ablauf:

        - Wird gestartet
        - Holt sich Events aus Event Store
        - baut projection auf

        - Events kommen an
        - Readmodel entscheidet spezifisch wie es den state (für stream oder alle) holt (sql query, aus memory etc)
        - Readmodel schickt state an projection
        - projection generiert neuen State
        - Readmodel hinterlegt state

        - Query kommt an
        - Readmodel prüft, ob es damit umgehen kann
        - Readmodel entscheidet wie es damit umgeht
        - gibt antwort zurück

        Vorteil Memory: nichts anderes zu deployen etc
        Nachteil: Serverstart, Speicherverbrauch

        Zeigen: Behaviour kommt später, wir haben keinen Zugriff drauf

        Man kann mehr logik in ein Readmodel bauen oder unterschiedliche
    *)

  type Msg<'Event,'Query,'Result> =
    | Notify of StreamEvent<'Event>
    | Query of 'Query * AsyncReplyChannel<QueryResult<'Result>>

  let flavoursInStock () =
    let agent =
      let initState : Map<Stream, Map<Flavour, int>> = Map.empty  // hier record draus machen

      MailboxProcessor.Start(fun inbox -> 
        let rec loop state =
          async {
            let! msg = inbox.Receive() 

            match msg with  
            | Notify event ->
                let newState =
                  state
                  |> Map.tryFind event.Stream
                  |> Option.defaultValue Projections.flavoursInStock.Init
                  |> fun projectionState -> event.Event |> Projections.flavoursInStock.Update projectionState
                  |> fun newState -> state |> Map.add event.Stream newState

                return! loop newState

            | Query (query, reply) ->
                let result =
                  match query with  
                  | FlavoursInStock (Truck truck, flavour) ->
                      state 
                      |> Map.tryFind truck
                      |> Option.defaultValue Map.empty 
                      |> Map.tryFind flavour 
                      |> Option.defaultValue 0 
                      |> Handled 

                   | _ -> 
                      NotHandled

                result |> reply.Reply                                  
          }

        loop initState        
      )

    {
      EventListener = Notify >> agent.Post
      QueryHandler = fun query -> agent.PostAndReply(fun reply -> Query (query,reply))
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
  open EventStore

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
    |> List.length
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
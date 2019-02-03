namespace Domain
open Infrastructure

type Flavour =
| Vanilla
| Strawberry

type Event =
| Flavour_sold of Flavour
| Flavour_restocked of Flavour * int
| Flavour_went_out_of_stock of Flavour
| Flavour_was_not_in_stock of Flavour

type Truck = Truck of System.Guid


module API =

  type Query =
    | Trucks
    | FlavoursInStock of Truck * Flavour


  // type QueryResult =
  //   | QueryResult of obj


module Projections =

  open Infrastructure

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


module Readmodels =
  open API
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
                      |> box
                      |> Handled

                  | Trucks ->
                      [
                        System.Guid.NewGuid() |> Truck
                      ]
                      |> box
                      |> Handled

                   | _ ->
                      NotHandled

                result |> reply.Reply

                return! loop state
          }

        loop initState
      )

    {
      EventListener = Notify >> agent.Post
      QueryHandler = fun query -> agent.PostAndReply(fun reply -> Query (query,reply))
    }


module Behaviour =

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

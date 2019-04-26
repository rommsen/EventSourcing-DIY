namespace Application

module API =

  open Domain

  type Query =
    | Trucks
    | FlavourInStockOfTruck of Truck * Flavour
    | FlavourInStockOfAll of Flavour
    | FlavoursSoldOfTruck of Truck * Flavour
    | FlavoursSoldOfAll of Flavour

module InMemoryReadmodels =
  open Infrastructure
  open Agent
  open Domain

  type Msg<'Event,'Result> =
    | Notify of EventEnvelope<'Event> list * AsyncReplyChannel<unit>
    | State of AsyncReplyChannel<'Result>

  let projectIntoMap projection =
    fun state eventEnvelope ->
      state
      |> Map.tryFind eventEnvelope.Metadata.Source
      |> Option.defaultValue projection.Init
      |> fun projectionState -> eventEnvelope.Event |> projection.Update projectionState
      |> fun newState -> state |> Map.add eventEnvelope.Metadata.Source newState

  let readModel (updateState : 'State -> EventEnvelope<'Event> list -> 'State) (initState : 'State) : ReadModel<'Event, 'State> =
    let agent =
      let eventSubscriber (inbox : Agent<Msg<_,_>>) =
        let rec loop state =
          async {
            let! msg = inbox.Receive()

            match msg with
            | Notify (eventEnvelopes, reply) ->
                reply.Reply ()
                return! loop (eventEnvelopes |> updateState state)

            | State reply ->
                reply.Reply state
                return! loop state
          }

        loop initState

      Agent<Msg<_,_>>.Start(eventSubscriber)

    {
      EventHandler = fun eventEnvelopes -> agent.PostAndAsyncReply(fun reply -> Notify (eventEnvelopes,reply))
      State = fun () -> agent.PostAndAsyncReply State
    }


  let flavoursInStock () : ReadModel<_,_> =
    let updateState state eventEnvelopes =
      eventEnvelopes
      |> List.fold (projectIntoMap Projections.flavoursInStock) state

    readModel updateState Map.empty

  let flavoursSold () : ReadModel<_,_> =
    let updateState state eventEnvelopes =
      eventEnvelopes
      |> List.fold (projectIntoMap Projections.soldFlavours) state

    readModel updateState Map.empty


module PersistentReadmodels =
  open Infrastructure
  open Domain
  open Npgsql.FSharp

  let parameters eventEnvelope =
    match eventEnvelope.Event with
    | Flavour_sold (Truck truck,flavour) ->
        [
          "@truck", SqlValue.Uuid truck
          "@flavour", SqlValue.String (Flavour.toString flavour)
        ] |> Some

    | _ -> None

  let flavourSoldHandler (DB_Connection_String db_connection) : EventHandler<Event> =
    let query = """
      INSERT INTO flavours_sold (truck, flavour, sold) VALUES (@truck, @flavour, 1)
      ON CONFLICT (truck,flavour) DO UPDATE SET sold = flavours_sold.sold + 1"""

    fun eventEnvelopes ->
      let parameters =
        eventEnvelopes |> List.choose parameters

      if not <| List.isEmpty parameters then
        db_connection
        |> Sql.connect
        |> Sql.executeTransactionAsync [ query, parameters ]
        |> Async.Ignore
      else
        async { return () }


module QueryHandlers =
  open API
  open Domain
  open Infrastructure
  open Npgsql.FSharp

  let flavours flavoursInStock (DB_Connection_String db_connection) =
    let handleQuery query =
      match query with
      | FlavourInStockOfTruck(Truck truck, flavour) ->
          async {
            let! state = flavoursInStock()

            return
              state
              |> Map.tryFind truck
              |> Option.defaultValue Map.empty
              |> Map.tryFind flavour
              |> Option.defaultValue 0
              |> box
              |> Handled
          }

      | FlavourInStockOfAll flavour ->
          async {
            let! state = flavoursInStock()

            return
              state
              |> Map.fold (fun total _ stockOfTruck ->
                  stockOfTruck
                  |> Map.tryFind flavour
                  |> Option.defaultValue 0
                  |> fun stock -> stock + total) 0
              |> box
              |> Handled
          }

      | FlavoursSoldOfTruck (Truck truck, flavour) ->
          async {
            return
              db_connection
              |> Sql.connect
              |> Sql.query "SELECT sold FROM flavours_sold WHERE truck = @truck AND flavour = @flavour"
              |> Sql.parameters [ "@truck", SqlValue.Uuid truck ; "@flavour", SqlValue.String (Flavour.toString flavour) ]
              |> Sql.executeScalarSafe
              |> function | Ok (SqlValue.Int sold) -> sold | _ ->  0
              |> box
              |> Handled
          }

      | FlavoursSoldOfAll flavour ->
          async {
            return
              db_connection
              |> Sql.connect
              |> Sql.query "SELECT SUM(sold) :: int FROM flavours_sold WHERE flavour = @flavour"
              |> Sql.parameters [ "@flavour", SqlValue.String (Flavour.toString flavour) ]
              |> Sql.executeScalarSafe
              |> function | Ok (SqlValue.Int sold) -> sold | _ -> 0
              |> box
              |> Handled
          }

       | _ ->
          async { return NotHandled }


    { Handle = handleQuery }

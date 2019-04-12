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
  open Domain

  type Msg<'Event,'Result> =
    | Notify of EventEnvelope<'Event>
    | State of AsyncReplyChannel<'Result>

  let flavoursInStock () : ReadModel<_,_> =
    let agent =
      let initState : Map<EventSource, Map<Flavour, int>> = Map.empty

      let eventSubscriber (inbox : Agent<Msg<_,_>>) =
        let rec loop state =
          async {
            let! msg = inbox.Receive()

            match msg with
            | Notify eventEnvelope ->
                let newState =
                  state
                  |> Map.tryFind eventEnvelope.Source
                  |> Option.defaultValue Projections.flavoursInStock.Init
                  |> fun projectionState -> eventEnvelope.Event |> Projections.flavoursInStock.Update projectionState
                  |> fun newState -> state |> Map.add eventEnvelope.Source newState

                return! loop newState

            | State reply ->
                reply.Reply state
                return! loop state
          }

        loop initState

      Agent<Msg<_,_>>.Start(eventSubscriber)

    {
      EventListener = Notify >> agent.Post
      State = fun () -> agent.PostAndAsyncReply State
    }

  let flavoursSold () : ReadModel<_,_> =
    let agent =
      let eventSubscriber (inbox : Agent<Msg<_,_>>) =
        let rec loop (state : Map<EventSource, Map<Flavour, int>>) =
          async {
            let! msg = inbox.Receive()

            match msg with
            | Notify eventEnvelope ->
                let newState =
                  state
                  |> Map.tryFind eventEnvelope.Source
                  |> Option.defaultValue Projections.flavoursInStock.Init
                  |> fun projectionState -> eventEnvelope.Event |> Projections.flavoursInStock.Update projectionState
                  |> fun newState -> state |> Map.add eventEnvelope.Source newState

                return! loop newState

            | State reply ->
                reply.Reply state
                return! loop state
          }

        loop Map.empty

      Agent<Msg<_,_>>.Start(eventSubscriber)

    {
      EventListener = Notify >> agent.Post
      State = fun () -> agent.PostAndAsyncReply State
    }


module PersistentReadmodels =
  open Infrastructure
  open Domain
  open Npgsql.FSharp

  let flavourSoldListener (DB_Connection_String db_connection) : EventListener<Event> =
    fun eventEnvelope ->
      match eventEnvelope.Event with
      | Flavour_sold (Truck truck,flavour) ->
          let query = """
              INSERT INTO flavours_sold (truck, flavour, sold) VALUES (@truck, @flavour, 1)
  	          ON CONFLICT (truck,flavour) DO UPDATE SET sold = flavours_sold.sold + 1"""

          let parameter = [
            [ "@truck", SqlValue.Uuid truck
              "@flavour", SqlValue.String (Flavour.toString flavour) ] ]

          db_connection
          |> Sql.connect
          |> Sql.executeTransaction [ query, parameter ]
          |> ignore

      | _ -> ()



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
                  |> (+) total) 0
              |> box
              |> Handled
          }

      | FlavoursSoldOfTruck (Truck truck, flavour) ->
          async {
            return
              db_connection
              |> Sql.connect
              |> Sql.query "SELECT sold FROM flavours_sold WHERE truck = @truck AND flavour = @flavour"
              |> Sql.parameters [ "@truck", SqlValue.Uuid truck ; "@flavour" , SqlValue.String (Flavour.toString flavour) ]
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
              |> Sql.parameters [ "@flavour" , SqlValue.String (Flavour.toString flavour) ]
              |> Sql.executeScalarSafe
              |> function | Ok (SqlValue.Int sold) -> sold | _ -> 0
              |> box
              |> Handled
          }

       | _ ->
          async { return NotHandled }


    { Handle = handleQuery }

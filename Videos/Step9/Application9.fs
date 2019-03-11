namespace Step9.Application

module API =

  open Step9.Domain

  type Query =
    | Trucks
    | FlavourInStockOfTruck of Truck * Flavour
    | FlavourInStockOfAll of Flavour

module InMemoryReadmodels =
  open Step9.Infrastructure
  open Step9.Domain

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


  // let trucks () =
  //   let agent =
  //     let initState = []

  //     Agent<Msg<_,_,_>>.Start(fun inbox ->
  //       let rec loop state =
  //         async {
  //           let! msg = inbox.Receive()

  //           match msg with
  //           | Notify event ->
  //               return! loop state

  //           | Query (query, reply) ->
  //               let result =
  //                 match query with
  //                 | Trucks ->
  //                     [ System.Guid.NewGuid() |> Truck ]
  //                     |> box
  //                     |> Handled

  //                  | _ ->
  //                     NotHandled

  //               result |> reply.Reply

  //               return! loop state
  //         }

  //       loop initState
  //     )

  //   {
  //     EventListener = Notify >> agent.Post
  //     QueryHandler = { Handle = fun query -> agent.PostAndAsyncReply(fun reply -> Query (query,reply)) }
  //     OnError = agent.OnError
  //   }


module QueryHandlers =
  open API
  open Step9.Domain
  open Step9.Infrastructure

  let flavours flavourState =
    let handleQuery query =
      match query with
      | FlavourInStockOfTruck(Truck truck, flavour) ->
          async {
            let! state = flavourState()

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
            let! state = flavourState()

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

       | _ ->
          async { return NotHandled }


    { Handle = handleQuery }

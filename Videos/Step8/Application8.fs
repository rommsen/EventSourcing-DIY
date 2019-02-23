namespace Step8.Application

module API =

  open Step8.Domain

  type Query =
    | Trucks
    | FlavoursInStock of Truck * Flavour


module Readmodels =
  open Step8.Infrastructure
  open Step8.Domain
  open API

  type Msg<'Event,'Query,'Result> =
    | Notify of EventEnvelope<'Event>
    | Query of 'Query * AsyncReplyChannel<QueryResult<'Result>>

  let flavoursInStock () =
    let agent =
      let initState : Map<EventSource, Map<Flavour, int>> = Map.empty  // hier record draus machen

      MailboxProcessor.Start(fun inbox ->
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

  let trucks () =
    let agent =
      let initState = []

      MailboxProcessor.Start(fun inbox ->
        let rec loop state =
          async {
            let! msg = inbox.Receive()

            match msg with
            | Notify event ->
                return! loop state

            | Query (query, reply) ->
                let result =
                  match query with
                  | Trucks ->
                      [ System.Guid.NewGuid() |> Truck ]
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
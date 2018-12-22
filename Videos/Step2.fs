module EventStore =

  type EventStore<'Event> =
    {
      Get : unit -> 'Event list
      Append : 'Event list -> unit
    }

  type Msg<'Event> =
    | Get of AsyncReplyChannel<'Event list>
    | Append of 'Event list

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
          }

        loop history
      )

    let append events =
      events
      |> Append
      |> mailbox.Post

    {
      Get = fun () ->  mailbox.PostAndReply Get
      Append = append
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

module Helper =
  let printUl list =
    list
    |> List.iteri (fun i item -> printfn " %i: %A" (i+1) item)

  let printEvents  events =
    events
    |> List.length
    |> printfn "History (Length: %i)"

    events |> printUl


  let soldOfFlavour flavour state =
    state
    |> Map.tryFind flavour
    |> Option.defaultValue 0


  let printSoldFlavour flavour state =
    state
    |> soldOfFlavour flavour
    |> printfn "Sold %A: %i" flavour


open Domain
open EventStore
open Projections
open Helper

[<EntryPoint>]
let main _ =

  let eventStore : EventStore<Event> = EventStore.initialize()

  eventStore.Append [Flavour_restocked (Vanilla,3)]

  eventStore.Append [Flavour_sold Vanilla]
  eventStore.Append [Flavour_sold Vanilla]
  eventStore.Append [Flavour_sold Vanilla ; Flavour_went_out_of_stock Vanilla]


  let events = eventStore.Get()

  events
  |> printEvents

  let sold =
    events
    |> project soldFlavours

  printSoldFlavour Vanilla sold
  printSoldFlavour Strawberry sold

  0
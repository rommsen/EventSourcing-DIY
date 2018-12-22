module EventStore =

  type EventStore<'Event> =
    {
      Get : unit -> 'Event list
      Append : 'Event list -> unit
    }


  // what can the MB do
  type Msg<'Event> =
    | Append of 'Event list
    | Get of AsyncReplyChannel<'Event list> // what kind of reply do we expect

  let initialize () : EventStore<'Event> =
    let agent = 
      MailboxProcessor.Start(fun inbox ->
        // state can be any state the agent should store
        let rec loop history = 
          async {
            // let! is like await
            let! msg = inbox.Receive() 

            match msg with  
            | Append events ->
                // call the recursive function to let the agent live
                return! loop (history @ events)  

            | Get reply ->
                // reply on the given channel 
                reply.Reply history 

                // call the recursive function to let the agent live
                return! loop history        
          }

        loop []   
      )

    let append events =
      agent.Post (Append events)

    let get () =
      agent.PostAndReply Get

    {
      Get = get 
      Append = append
    }      


module Domain =

  type Flavour =
    | Strawberry 
    | Vanilla 

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

  let project (projection : Projection<_,_>) events =
    events |> List.fold projection.Update projection.Init  


  let soldOfFlavour flavour state =
    state  
    |> Map.tryFind flavour 
    |> Option.defaultValue 0

  let updateSoldFlavours state event = 
    match event with  
    | Flavour_sold flavour ->
        // do something with the sate 
        state  // Map<Flavour,int>
        |> soldOfFlavour flavour 
        |> fun portions -> state |> Map.add flavour (portions + 1)

    | _ -> state        

  let soldFlavours : Projection<Map<Flavour,int>,Event> =
    {
      Init = Map.empty 
      Update = updateSoldFlavours
    }





module Helper =
  open Projections

  let printUl list =
    list
    |> List.iteri (fun i item -> printfn " %i: %A" (i+1) item)

  let printEvents events =
    events
    |> List.length
    |> printfn "History (Length: %i)"

    events |> printUl

  let printSoldFlavour flavour state =
    state
    |> soldOfFlavour flavour
    |> printfn "Sold %A: %i" flavour  


open EventStore
open Projections
open Domain
open Helper

[<EntryPoint>]
let main _ =
  

  let eventStore : EventStore<Event> = EventStore.initialize()

  eventStore.Append [Flavour_restocked (Vanilla,3)]
  eventStore.Append [Flavour_sold Vanilla]
  eventStore.Append [Flavour_sold Vanilla]
  eventStore.Append [Flavour_sold Vanilla ; Flavour_went_out_of_stock Vanilla]

  let events = eventStore.Get() 
  
  
  events |> printEvents


  (*
  Map {
    Vanilla = 0 
    Strawberry = 5
  }
  *)

  let sold : Map<Flavour,int> =
    events |> project soldFlavours

  printSoldFlavour Vanilla sold 
  printSoldFlavour Strawberry sold 
  

  0
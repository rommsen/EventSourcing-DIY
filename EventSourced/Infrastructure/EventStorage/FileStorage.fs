namespace Infrastructure.EventStorage
open Infrastructure

module FileStorage =

  open System.IO
  open Thoth.Json.Net

  let private get store =
    store
    |> File.ReadLines
    |> List.ofSeq
    |> List.traverseResult Decode.Auto.fromString<EventEnvelope<'Event>>

  let private getStream store source =
    store
    |> File.ReadLines
    |> List.ofSeq
    |> List.traverseResult Decode.Auto.fromString<EventEnvelope<'Event>>
    |> Result.map (List.filter (fun ee -> ee.Metadata.Source = source))

  let private append store events =
    use streamWriter = new StreamWriter(store, true)
    events
    |> List.map (fun eventEnvelope -> Encode.Auto.toString(0,eventEnvelope))
    |> List.iter streamWriter.WriteLine

    do streamWriter.Flush()


  let initialize store : EventStorage<_> =
    {
      Get = fun () -> async { return get store }
      GetStream = fun eventSource -> async { return getStream store eventSource  }
      Append = fun events -> async { return append store events }
    }
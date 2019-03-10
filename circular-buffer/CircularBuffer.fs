module CircularBuffer

type Buffer = {size: int; elements: int list}

let mkCircularBuffer x = { size = x; elements = []}

let clear buffer = { size = buffer.size; elements = []}

let write value buffer =
    match buffer.elements.Length = buffer.size with
    | true -> System.Exception() |> raise
    | _ -> { size = buffer.size; elements = value::(buffer.elements |> List.rev) |> List.rev  }

let read buffer = 
    match buffer.elements with
    | [] -> System.Exception() |> raise
    | head::tail -> (head, { size = buffer.size; elements = tail })

let forceWrite value buffer =
    match buffer.elements.Length = buffer.size with
    | false -> write value buffer
    | _ -> read buffer |> snd |> write value
module CircularBuffer

type Buffer = {size: int; elements: int list}

let mkCircularBuffer x = { size = x; elements = []}

let clear buffer = { buffer with elements = [] }

let write value buffer =
    match buffer.elements.Length = buffer.size with
    | true -> System.Exception() |> raise
    | _ -> { buffer with elements = buffer.elements @ [value] }

let read buffer = 
    match buffer.elements with
    | [] -> System.Exception() |> raise
    | head::tail -> (head, { buffer with elements = tail })

let forceWrite value buffer =
    match buffer.elements.Length = buffer.size with
    | false -> write value buffer
    | _ -> read buffer |> snd |> write value
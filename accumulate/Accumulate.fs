module Accumulate

let rec accumulate<'a, 'b> (func: 'a -> 'b) (input: 'a list): 'b list = 
    // input |> List.map func
    if input.IsEmpty then List.Empty
    else accumulate func input.Tail |> List.append [func input.[0]]
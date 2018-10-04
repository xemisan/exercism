module Accumulate

let rec accumulate<'a, 'b> (func: 'a -> 'b) (input: 'a list): 'b list = 
    match input with
    | [] -> List.empty
    | _ -> func input.[0]::accumulate func input.Tail
module Accumulate

let rec accumulate<'a, 'b> (func: 'a -> 'b) (input: 'a list): 'b list = 
    match input with
    | [] -> List.empty
    | head::tail -> func head::accumulate func tail
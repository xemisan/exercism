module Accumulate

let accumulate<'a, 'b> (func: 'a -> 'b) (input: 'a list): 'b list = 
    let rec loop accumulator = function
        | [] -> List.rev accumulator
        | head::tail -> loop (func head::accumulator) tail 
        
    loop [] input
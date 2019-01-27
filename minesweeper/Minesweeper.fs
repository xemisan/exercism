module Minesweeper

let transform (row: int) (column: int) (input: string list) =
    let topLeft = row-1 > -1 && column-1 > -1 && input.[row-1].[column-1] = '*'
    let topCenter = row-1 > -1 && input.[row-1].[column] = '*'
    let topRight = row-1 > -1 && column+1 < input.[row].Length && input.[row-1].[column+1] = '*'

    let left = column-1 > -1 && input.[row].[column-1] = '*'
    let right = column+1 < input.[row].Length && input.[row].[column+1] = '*'

    let bottomLeft = row+1 < input.Length && column-1 > -1 && input.[row+1].[column-1] = '*'
    let bottomCenter = row+1 < input.Length && input.[row+1].[column] = '*'
    let bottomRight = row+1 < input.Length && column+1 < input.[row].Length && input.[row+1].[column+1] = '*'
    
    [topLeft; topCenter; topRight; left; right; bottomLeft; bottomCenter; bottomRight]
    |> List.map (fun x -> if x then 1 else 0) 
    |> List.sum
    |> function
        | x when x = 0 -> ' '
        | x -> x.ToString() |> System.Char.Parse

let annotate (input: string list) : string list =
    [| for i in 0 .. input.Length - 1 -> 
        [| for j in 0 .. input.[i].Length - 1 ->
            match input.[i].[j] with
            | '*' -> '*'
            | ' ' -> transform i j input
            | _ -> failwith "Invalid input"
        |]
        |> System.String.Concat
    |]
    |> Array.toList
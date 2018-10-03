module Hamming

let distance (strand1: string) (strand2: string): int option = 
    if strand1.Length <> strand2.Length then None
    else
        let count = [| for i in 0 .. strand1.Length-1 -> strand1.[i] = strand2.[i] |] |> 
                        Array.filter (fun x -> x = false) |> 
                            Array.length
        Some count
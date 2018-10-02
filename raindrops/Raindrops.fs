module Raindrops

let sounds = dict[3, "Pling"; 5, "Plang"; 7, "Plong"]

let addDefault number input = if input = System.String.Empty then number.ToString() else input

let convert (number: int): string = 
    sounds.Keys |> 
        Seq.filter(fun f -> number % f = 0) |>
            Seq.map (fun f -> sounds.Item(f)) |> 
                System.String.Concat |> 
                    addDefault number
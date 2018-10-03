module Raindrops

let sounds = [(3, "Pling"); (5, "Plang"); (7, "Plong")]

let addDefault number input = if input = System.String.Empty then number.ToString() else input

let convert (number: int): string = 
    sounds |> 
        Seq.filter(fun (factor,_) -> number % factor = 0) |> 
            Seq.map (fun (_,sound) -> sound) |> 
                System.String.Concat |> 
                    addDefault number
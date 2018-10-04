module Raindrops

let sounds = [(3, "Pling"); (5, "Plang"); (7, "Plong")]

let convert (number: int): string = 
    sounds
    |> Seq.filter(fun (factor,_) -> number % factor = 0)
    |> Seq.map snd
    |> function
        | result when Seq.isEmpty result -> number.ToString()
        | result -> System.String.Concat result
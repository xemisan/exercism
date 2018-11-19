module Allergies

open System

type Allergen = Eggs = 1 | Peanuts = 2 | Shellfish = 4 | Strawberries = 8 | Tomatoes = 16 | Chocolate = 32 | Pollen = 64 | Cats = 128

let decodeAllergen value : Allergen = LanguagePrimitives.EnumOfValue value

let rec list codedAllergies = 
        match codedAllergies with
        | 0 -> []
        | _ ->
                let exp = Math.Log(float codedAllergies, 2.) |> floor |> int
                let number = pown 2 exp
                
                match Enum.IsDefined(typeof<Allergen>, number) with
                | true ->
                        let head = decodeAllergen number
                        let tail = list (codedAllergies-number) |> List.rev
                        head::tail |> List.rev
                | false -> list (codedAllergies-number)

let allergicTo codedAllergies allergen = list codedAllergies |> List.contains allergen
module RomanNumerals

let units = [""; "I"; "II"; "III"; "IV"; "V"; "VI"; "VII"; "VIII"; "IX"]

let tens = [""; "X"; "XX"; "XXX"; "XL"; "L"; "LX"; "LXX"; "LXXX"; "XC"]

let hundreds = [""; "C"; "CC"; "CCC"; "CD"; "D"; "DC"; "DCC"; "DCCC"; "CM"]

let rec romanRecursive arabicNumeral romanNumeral =
    match arabicNumeral > 1000, arabicNumeral > 100, arabicNumeral > 10 with
    | true, _, _ ->
        let thousands = [| for _ in 1 .. arabicNumeral / 1000 -> "M" |] |> System.String.Concat
        let pending = arabicNumeral % 1000
        romanRecursive pending thousands
    | _, true, _ ->
        let index = arabicNumeral / 100
        let pending = arabicNumeral % 100
        romanRecursive pending (romanNumeral + hundreds.[index])
    | _, _, true ->
        let index = arabicNumeral / 10
        let pending = arabicNumeral % 10
        romanRecursive pending (romanNumeral + tens.[index])
    | _, _, _ ->
        romanNumeral + units.[arabicNumeral]

let roman arabicNumeral = romanRecursive arabicNumeral ""
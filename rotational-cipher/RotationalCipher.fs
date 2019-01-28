module RotationalCipher

let isUpperCase number =
    number >= 65 && number <= 90
    
let isLowerCase number =
    number >= 97 && number <= 122

let convert (k: int) (c:char): char =
    let value = (int c)
    let newValue = value + k

    match (isUpperCase value, newValue > 90, isLowerCase value, newValue > 122) with
    | false, _, false, _ -> c
    | true, true, _, _ -> char (newValue - 26)
    | _, _, true, true -> char (newValue - 26)
    | _, _, _, _ -> char newValue

let rotate (shiftKey: int) (text: string) =
    text
    |> Seq.map (fun c -> convert shiftKey c )
    |> System.String.Concat
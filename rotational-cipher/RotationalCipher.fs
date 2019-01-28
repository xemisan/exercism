module RotationalCipher

open System

let rotateLetter (k: int) (c:char): char =
    let value = (int c) + k
    match (Char.IsLetter c, Char.IsUpper c, Char.IsLower c, value > 90, value > 122) with
    | false, _, _, _, _ -> c
    | _, true, _, true, _ -> char (value - 26)
    | _, _, true, _, true -> char (value - 26)
    | _, _, _, _, _ -> char value

let rotate (shiftKey: int) (text: string) =
    text
    |> Seq.map (rotateLetter shiftKey)
    |> String.Concat
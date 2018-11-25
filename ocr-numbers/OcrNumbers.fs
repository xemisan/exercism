module OcrNumbers

open System

[<StructuredFormatDisplay("{Text}")>]
type Number(l1:string, l2: string, l3: string, l4: string) = 
    member this.firstLine = l1
    member this.secondLine = l2
    member this.thirdLine = l3
    member this.fourthLine = l4
    member this.Text = sprintf "\n%s\n%s\n%s\n%s" this.firstLine this.secondLine this.thirdLine this.fourthLine
    override x.GetHashCode() =
        hash (l1, l2, l3, l4)
    override x.Equals(b) =
        match b with
        | :? Number as n -> (l1, l2, l3, l4) = (n.firstLine, n.secondLine, n.thirdLine, n.fourthLine)
        | _ -> false

let one = new Number("   ", "  |", "  |", "   ")
let two = new Number(" _ ", " _|", "|_ ", "   ")
let three = new Number(" _ ", " _|", " _|", "   ")
let four = new Number("   ", "|_|", "  |", "   ")
let five = new Number(" _ ", "|_ ", " _|", "   ")
let six = new Number(" _ ", "|_ ", "|_|", "   ")
let seven = new Number(" _ ", "  |", "  |", "   ")
let eight = new Number(" _ ", "|_|", "|_|", "   ")
let nine = new Number(" _ ", "|_|", " _|", "   ")
let zero = new Number(" _ ", "| |", "|_|", "   ")

let decode number =
    match number = one, number = two, number = three, number = four, number = five, number = six, number = seven, number = eight, number = nine, number = zero with
    | true, _, _, _, _, _, _, _, _, _ -> '1'
    | _, true, _, _, _, _, _, _, _, _ -> '2'
    | _, _, true, _, _, _, _, _, _, _ -> '3'
    | _, _, _, true, _, _, _, _, _, _ -> '4'
    | _, _, _, _, true, _, _, _, _, _ -> '5'
    | _, _, _, _, _, true, _, _, _, _ -> '6'
    | _, _, _, _, _, _, true, _, _, _ -> '7'
    | _, _, _, _, _, _, _, true, _, _ -> '8'
    | _, _, _, _, _, _, _, _, true, _ -> '9'
    | _, _, _, _, _, _, _, _, _, true -> '0'
    | _, _, _, _, _, _, _, _, _, _ -> '?'

let createNumbers (input: string list) : Number list =
    [| for i in 0 .. 3 .. input.[0].Length - 1 -> new Number(input.[0].Substring(i, 3), input.[1].Substring(i, 3), input.[2].Substring(i, 3), input.[3].Substring(i, 3)) |]
    |> List.ofArray
    
let convert (input: string list) : string option =
    let rowLengths = input |> List.map (fun x -> x.Length) |> List.distinct
    match input.Length % 4 = 0, rowLengths.[0] % 3 = 0, rowLengths.Length = 1 with
    | false, _, _ -> None
    | _, false, _ -> None
    | _, _, false -> None
    | true, true, true ->
        input
        |> (fun x -> [| for i in 0 .. 4 .. x.Length - 1 -> x.[i..i+3] |])
        |> Array.map createNumbers
        |> Array.map (fun numbers -> numbers |> List.map decode)
        |> Array.map (fun chars -> new String(chars |> Array.ofList))
        |> String.concat ","
        |> Some
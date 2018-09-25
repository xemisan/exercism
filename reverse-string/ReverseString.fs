module ReverseString

let reverse (input: string): string =
    System.String.Concat([| for i in input.Length-1 .. -1 .. 0 -> input.[i] |])
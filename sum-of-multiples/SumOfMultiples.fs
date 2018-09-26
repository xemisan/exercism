module SumOfMultiples

let keepNumbersWithDivisors (divisors: int list) (number: int): int =
    let isDivisible = divisors |> List.filter(fun d -> number % d = 0) |> List.tryHead
    // printfn "%A contains any divisor of %d? %b" divisors number isDivisible.IsSome
    match isDivisible with
            | Some _ -> number
            | None -> 0

let sum (numbers: int list) (upperBound: int): int =
    List.sumBy (keepNumbersWithDivisors numbers) [1..upperBound-1]
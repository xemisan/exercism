module SumOfMultiples

let keepNumbersWithDivisors (divisors: int list) (number: int): int =
    let isDivisible = divisors |> List.tryFind(fun d -> number % d = 0)
    match isDivisible with
            | Some _ -> number
            | None -> 0

let sum (numbers: int list) (upperBound: int): int =
    List.sumBy (keepNumbersWithDivisors numbers) [1..upperBound-1]
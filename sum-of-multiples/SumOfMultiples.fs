module SumOfMultiples

let keepNumberIfDivisible number divisor  = 
    if number % divisor = 0 then number
    else 0

let cleanNumbersThatDontHaveDivisors (divisors: int list) (number: int): int =
    let list = divisors |> List.map (keepNumberIfDivisible number) |> List.filter ((<>)0)
    if list.Length > 0 then list.[0]
    else 0
    
let sum (numbers: int list) (upperBound: int): int =
    [1..upperBound-1] |> List.map (cleanNumbersThatDontHaveDivisors numbers) |> List.sum
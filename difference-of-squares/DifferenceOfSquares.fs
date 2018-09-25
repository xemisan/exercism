module DifferenceOfSquares

let squareOfSum (number: int): int = 
    let sum = List.sum [1 .. number]
    sum*sum

let sumOfSquares (number: int): int = 
    List.sumBy (fun elem -> elem*elem) [1 .. number]

let differenceOfSquares (number: int): int = 
    squareOfSum number - sumOfSquares number
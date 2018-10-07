module QueenAttack

let create (position: int * int) = 
    let row = fst position
    let column = snd position
    row >= 0 && row <=7 && column >= 0 && column <=7

let canAttack (queen1: int * int) (queen2: int * int) = 
    let row1 = fst queen1
    let column1 = snd queen1
    let row2 = fst queen2
    let column2 = snd queen2
    match create queen1, 
          create queen2, 
          row1 = row2, 
          column1 = column2,
          abs (row1 - row2) = abs (column1 - column2) with
    | true, true, true, _, _ -> true
    | true, true, _, true, _ -> true
    | true, true, _, _, true -> true
    | _, _, _, _, _ -> false
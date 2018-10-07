module BeerSong

let firstVerse number = 
    if number = 0 then
        sprintf "No more bottles of beer on the wall, no more bottles of beer."
    elif number = 1 then
        sprintf "%d bottle of beer on the wall, %d bottle of beer." number number
    else
        sprintf "%d bottles of beer on the wall, %d bottles of beer." number number

let secondVerse number = 
    if number = 0 then
        sprintf "Go to the store and buy some more, 99 bottles of beer on the wall."
    elif number = 1 then
        sprintf "Take it down and pass it around, no more bottles of beer on the wall."
    elif number = 2 then
        sprintf "Take one down and pass it around, 1 bottle of beer on the wall."
    else
        sprintf "Take one down and pass it around, %d bottles of beer on the wall." (number - 1)

let rec song startBottles takeDown accumulator = 
    match takeDown with
    | 0 -> accumulator |> List.tail |> List.rev
    | _ -> song (startBottles-1) (takeDown-1) (""::(secondVerse startBottles::(firstVerse startBottles::accumulator)))

let recite (startBottles: int) (takeDown: int) = song startBottles takeDown []
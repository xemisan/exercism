module Bob

open System.Text.RegularExpressions

let matches pattern input=
    let m = Regex.Match(input, pattern)
    m.Success

let isAsking = matches "\?[ \t]*$"
let isYelling = matches "^(?=.*[A-Z])[^a-z]+$"
let isAddressing = matches "^[^a-zA-Z0-9]*$"

let response (input: string): string = 
    match isAsking input, isYelling input, isAddressing input with
    | true, true, _ -> "Calm down, I know what I'm doing!"
    | true, false, _ -> "Sure."
    | false, true, _ -> "Whoa, chill out!"
    | false, false, true -> "Fine. Be that way!"
    | _, _, _ -> "Whatever."
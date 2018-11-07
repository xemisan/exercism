module Clock

open System

[<StructuredFormatDisplay("{Text}")>]
type Clock(ts:TimeSpan) = 
    member this.Hours = ts.Hours
    member this.Minutes = ts.Minutes
    member this.Text = sprintf "%02i:%02i" this.Hours this.Minutes
    member this.ToTimeSpan = TimeSpan.Parse this.Text
    override x.GetHashCode() =
        hash (ts.Hours, ts.Minutes)
    override x.Equals(b) =
        match b with
        | :? Clock as p -> (ts.Hours, ts.Minutes) = (p.Hours, p.Minutes)
        | _ -> false

let create hours minutes = 
    let result1 = minutes + (hours * 60) |> float |> TimeSpan.FromMinutes

    let result2 =  result1.Days |> abs |>  (+) 1 |> float |> TimeSpan.FromDays |> result1.Add
    
    if result2.Days = 0 then
        result2 |> Clock
    else
        result2.Days
        |> float
        |> TimeSpan.FromDays 
        |> match result2.Days > 0 with
           | true -> result2.Subtract
           | _ -> result2.Add
        |> Clock

let add (minutes:int) (clock:Clock) = 
    minutes |> float |> TimeSpan.FromMinutes |> clock.ToTimeSpan.Add |> Clock

let subtract (minutes:int) (clock:Clock) = 
    let result = minutes |> float |> TimeSpan.FromMinutes |> clock.ToTimeSpan.Subtract
    
    if result.Ticks < Int64.Parse("0") then
        let extraDays = float (result.Days |> abs |> (+) 1)
        
        let incrementado = extraDays |> TimeSpan.FromDays |> clock.ToTimeSpan.Add
        
        minutes |> float |> TimeSpan.FromMinutes |> incrementado.Subtract |> Clock
    else
        result |> Clock

let display (clock:Clock) = clock.Text
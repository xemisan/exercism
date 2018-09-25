module Leap

let leapYear (year: int): bool = 
    if year % 4 <> 0 then 
        false
    else if year % 100 <> 0 then 
            true
        else year % 400 = 0
module Gigasecond
open System

let add (beginDate: DateTime): DateTime = 
    beginDate.AddSeconds(float(pown 10 9))
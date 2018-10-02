module RnaTranscription

open System
let toRna (dna: string): string =
    let chain = dna |> Seq.toList |> List.map (fun nucleotide -> 
        match nucleotide with
            | 'C' -> 'G'
            | 'G' -> 'C'
            | 'T' -> 'A'
            | 'A' -> 'U'
            | _ -> '?') |> Seq.toList |> List.toArray

    new String(chain)
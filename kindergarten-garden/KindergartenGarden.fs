module KindergartenGarden

type Plant = Grass | Clover | Radishes | Violets

let garden = dict[('G', Plant.Grass); ('C', Plant.Clover); ('R', Plant.Radishes); ('V', Plant.Violets)]

let children = ["Alice"; "Bob"; "Charlie"; "David"; "Eve"; "Fred"; "Ginny"; "Harriet"; "Ileana"; "Joseph"; "Kincaid"; "Larry"]

let plants diagram student =
    let splitIndex = diagram |> Seq.toList |> List.findIndex (fun e -> e = '\n')
    let rows = diagram |> Seq.toList |> List.splitAt splitIndex
    let firstRow = fst rows
    let secondRow = snd rows |> List.tail
    let startIndex = List.findIndex (fun e -> e = student) children |> (*) 2
    let plant1 = firstRow.[startIndex]
    let plant2 = firstRow.[startIndex + 1]
    let plant3 = secondRow.[startIndex]
    let plant4 = secondRow.[startIndex + 1]
    
    [garden.[plant1]; garden.[plant2]; garden.[plant3]; garden.[plant4]]
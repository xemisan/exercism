module TreeBuilding

type Record = { RecordId: int; ParentId: int }

type Tree = 
    | Branch of int * Tree list
    | Leaf of int

let recordId t = 
    match t with
    | Branch (id, _) -> id
    | Leaf id -> id

let isBranch t = 
    match t with
    | Branch (_, _) -> true
    | Leaf _ -> false

let children t = 
    match t with
    | Branch (_, c) -> c
    | Leaf _ -> []

let createBranch records id = 
    records
    |> List.filter (fun x -> x.ParentId = id)
    |> List.map (fun x -> Leaf x.RecordId)

let rec otraPrueba (records: Record List) (tree: Tree List): Tree List = 
    printfn ""
    printfn "-----------------------------------------------------------------------------------"
    printfn ""
    printfn "-> Elementos del árbol actual: %d" tree.Length
    printfn "\t%A" tree
    printfn "-> Elementos por procesar: %d" records.Length
    
    let posiblesPadres = records |> List.map(fun x -> x.ParentId) |> List.distinct
    printfn "-> Posibles padres: %d" posiblesPadres.Length
    
    let futurasHojas = records |> List.filter (fun x -> posiblesPadres |> List.contains x.RecordId |> not )
    printfn "-> Futuras hojas: %d" futurasHojas.Length
    
    let futurasRamas = futurasHojas |> List.map (fun x -> x.ParentId) |> List.distinct
    printfn "-> Futuras ramas: %d" futurasRamas.Length

    // let helper (hojas: Tree List) =
    //     hojas |> List.tryFindIndex (fun x -> recordId x = id) tree |> function
    //     | Some value ->
    //         printfn "\tSI lo hemos encontrao en el indice %d" value
    //         let item = tree.[value]
    //         children item
    //     | None ->
    //         printfn "\tNO lo hemos encontrao"
    //         createBranch futurasHojas id

        // printfn "\tArrebuscando el elemento %d" id
        // let exists = List.tryFindIndex (fun x -> recordId x = id) tree
        // match exists with
        // | Some value ->
        //     printfn "\tSI lo hemos encontrao en el indice %d" value
        //     let item = tree.[value]
        //     children item
        // | None ->
        //     printfn "\tNO lo hemos encontrao"
        //     createBranch futurasHojas id
        




    
    let ramas = futurasRamas |> List.map (fun x -> Branch (x, x |> createBranch futurasHojas ))
    printfn "-> Ramas reales: %d" ramas.Length
    printfn "\t%A" ramas

    let excluir = futurasHojas |> List.map(fun x -> x.RecordId)
    printfn "-> Hay que excluir: %d" excluir.Length

    let pendientes = records |> List.filter (fun x -> excluir |> List.contains x.RecordId |> not )
    printfn "-> Elementos pendientes: %d" pendientes.Length

    match pendientes.Length with
    | 1 -> 
        printfn "\t-> Hay un elemento, ¿q tal el puto arbol? %d" tree.Length
        match tree with
        | [] ->
            match ramas with
            | [] -> [Leaf pendientes.[0].RecordId]
            | _ -> ramas
        | _ -> ramas
    | _ -> 
        otraPrueba pendientes ramas
        
let buildTree records = 
    records
    |> List.sortBy (fun x -> x.RecordId)
    |> function
        | [] -> failwith "Empty input"
        | result when result.[0].ParentId = 0 |> not -> failwith "Root node is invalid"
        | result when result.[0].RecordId = 0 |> not -> failwith "Root node is invalid"
        | result ->
            let arbolito = otraPrueba result []
            arbolito.[0]
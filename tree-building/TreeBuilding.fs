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

let validateContinuousList result = 
    let ids = result |> List.map (fun x -> x.RecordId)
    [| for i in 0 .. ids |> List.max -> i |] |> Array.toList |> List.forall (fun x -> ids |> List.contains x)

let rec buildTreeRecursive (records: Record List) (tree: Tree List): Tree List = 
    let leafs = records |> List.filter (fun x -> records |> List.map(fun x -> x.ParentId) |> List.distinct |> List.contains x.RecordId |> not )
    let pending = records |> List.filter (fun x -> leafs |> List.map(fun x -> x.RecordId) |> List.contains x.RecordId |> not )
    let branches = leafs 
                |> List.map (fun x -> x.ParentId) 
                |> List.distinct 
                |> List.map (fun x -> 
                    Branch (x, leafs 
                    |> List.filter (fun x1 -> x1.ParentId = x) 
                    |> List.map (fun x2 -> 
                        match tree |> List.tryFind (fun y -> x2.RecordId = recordId y) with
                        | None -> Leaf x2.RecordId
                        | Some item -> Branch (x2.RecordId, children item))))
                        
    match pending.Length with
    | 1 -> 
        match tree with
        | [] ->
            match branches with
            | [] -> [Leaf pending.[0].RecordId]
            | _ -> branches
        | _ -> branches
    | _ -> buildTreeRecursive pending branches

let buildTree records = 
    records
    |> List.sortBy (fun x -> x.RecordId)
    |> function
        | [] -> failwith "Empty input"
        | result when result.[0].ParentId = 0 |> not -> failwith "Root node is invalid"
        | result when result.[0].RecordId = 0 |> not -> failwith "Root node is invalid"
        | result when result |> List.exists (fun r -> r.RecordId <> 0 && (r.ParentId > r.RecordId || r.ParentId = r.RecordId)) -> failwith "Nodes with invalid parents"
        | result when validateContinuousList result |> not -> failwith "Non-continuous list"
        | result ->
            let tree = buildTreeRecursive result []
            tree.[0]
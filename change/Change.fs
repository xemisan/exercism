module Change

let findFewestCoins coins target = 
    let tryAddToStack stack coin =
        match (target - List.sum stack) < coin with
        | true  -> None
        | false -> coin :: stack |> List.sort |> Some

    let processCoinStack stack =  coins |> List.choose (tryAddToStack stack)

    let rec loop coinStacks =
        let findTargetCoinStack = coinStacks |> List.tryFind (List.sum >> (=) target)

        match findTargetCoinStack with
        | Some stack ->  stack |> List.sort |> Some
        | None when List.isEmpty coinStacks -> None
        | None ->
            coinStacks
            |> List.distinct
            |> List.collect processCoinStack
            |> loop

    loop [[]]
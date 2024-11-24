namespace Trees

type Tree<'t> =
    | Node of Tree<'t> list
    | Leaf of 't


module tFunctions =

    let rec tMap tree func = 
        match tree with 
        | Leaf value -> Leaf (func value)
        | Node children ->
            Node (List.map (fun child -> tMap child func) children)

    let rec tLeftFold tree func acc =
        match tree with     
        | Leaf value -> func acc value
        | Node children -> 
            List.fold (fun acc child -> tLeftFold child func acc) acc children

    let rec tRightFold tree func acc =
        match tree with 
        | Leaf value -> func acc value
        | Node children ->
            List.fold (fun acc child -> tRightFold child func acc) acc (List.rev children) 

    let rec tHighOfTree tree=
        match tree with
        | Leaf _ -> 1
        | Node children -> 
            let heights = List.map tHighOfTree children
            1 + List.fold max 0 heights
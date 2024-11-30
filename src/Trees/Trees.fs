namespace Trees

type Tree<'t> =
    | Node of Tree<'t> list
    | Leaf of 't


module Tree =

    let rec map func tree  = 
        match tree with 
        | Leaf value -> Leaf (func value)
        | Node children ->
            Node (List.map (fun child -> map func child) children)

    let rec leftFold func acc tree =
        match tree with     
        | Leaf value -> func acc value
        | Node children -> 
            List.fold (fun acc child -> leftFold func acc child) acc children

    let rec rightFold func acc tree =
        match tree with 
        | Leaf value -> func acc value
        | Node children ->
            List.fold (fun acc child -> rightFold func acc child) acc (List.rev children) 

    let rec high tree=
        match tree with
        | Leaf _ -> 1
        | Node children -> 
            let heights = List.map high children
            1 + List.fold max 0 heights
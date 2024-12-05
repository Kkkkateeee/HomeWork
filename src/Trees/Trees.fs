namespace Trees

type NonEmptyList<'t> =
    | Single of 't
    | Cons of 't * NonEmptyList<'t>

type Tree<'t> =
    | Node of Tree<'t> NonEmptyList
    | Leaf of 't


module NonEmptyList = 

    let rec map func list =
        match list with 
        | Single value -> Single (func value)
        | Cons(head, tail) -> Cons(func head, map func tail)

    let rec fold func acc list =
        match list with 
        | Single value -> func value acc
        | Cons(head, tail) -> 
            let newAcc = func head acc
            fold func newAcc tail

    let rec foldBack func list acc =
        match list with 
        | Single value -> func value acc
        | Cons(head, tail) -> foldBack func tail (func head acc)

    let rec max list =
        match list with 
        | Single x -> x
        | Cons (head, tail) ->
            let tailMax = max tail
            if head > tailMax then head else tailMax


module Tree =

    let rec map func tree  = 
        match tree with 
        | Leaf value -> Leaf (func value)
        | Node children ->
            Node (NonEmptyList.map (map func) children)

    let rec foldLeft func acc tree =
        match tree with 
        | Leaf value -> func acc value
        | Node children -> 
            NonEmptyList.fold (fun child acc -> foldLeft func acc child) acc children

    let rec foldRight func acc tree =
        match tree with 
        | Leaf value -> func acc value
        | Node children ->
            NonEmptyList.foldBack (fun child acc -> foldRight func acc child) children acc 

    let rec high tree=
        match tree with
        | Leaf _ -> 1
        | Node children -> 
            let heights = NonEmptyList.map high children
            1 + NonEmptyList.max heights

    
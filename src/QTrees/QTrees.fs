namespace QTrees

type QTree<'t> = 
    | Leaf of 't
    | Node of QTree<'t> * QTree<'t> * QTree<'t> * QTree<'t> 

    
module QTrees = 

    let rec map func qtree =
        match qtree with 
        | Leaf value -> Leaf (func value)
        | Node (nw,ne, se, sw) ->
            Node (map func nw, map func ne, map func se, map func sw)
    

    let rec map2 func qtree1 qtree2 =
        match qtree1, qtree2 with 
        | Leaf value1, Leaf value2 -> 
            Leaf (func value1 value2)

        | Leaf value, Node (nw, ne, se, sw) ->
            Node (
                map2 func (Leaf value) nw,
                map2 func (Leaf value) ne,
                map2 func (Leaf value) se,
                map2 func (Leaf value) sw 
            )

        | Node (nw, ne, se, sw), Leaf value ->
            Node (
                map2 func nw (Leaf value),
                map2 func ne (Leaf value),
                map2 func se (Leaf value),
                map2 func sw (Leaf value)
            )

        | Node (nw, ne, se, sw), Node (NW, NE, SE, SW)  ->
            Node (
                map2 func nw NW,
                map2 func ne NE,
                map2 func se SE,
                map2 func sw SW
            )


    // let rec mult qtree1 qtree2 (opMult: 't -> 't -> 't)  =
    //     match qtree1, qtree2 with 
    //     | Leaf value1, Leaf value2 -> 
    //         Leaf(opMult value1 value2)
    //     | Leaf value, Node (nw, ne, se, sw) ->
    //         Node (mult value nw, mult value nw, mult value nw, mult value nw)
        
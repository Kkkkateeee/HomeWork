namespace Q_Trees

type Qtree<'t> = 
    | Leaf of 't
    | Node of Qtree<'t> * Qtree<'t> * Qtree<'t> * Qtree<'t> 

    
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


    let mult qtree1 qtree2 =
        let a = 1
        a
namespace QTrees 

 
type QTree<'t> =  
    | Leaf of 't * int
    | Node of QTree<'t> * QTree<'t> * QTree<'t> * QTree<'t>  
 
     
module QTrees =  

    let rec private toCorrectQTree qtree  =
        match qtree with 
        | Leaf (value, size) -> Leaf (value, size)
        
        | Node (nw, ne, se, sw) ->
            let NW = toCorrectQTree nw
            let NE = toCorrectQTree ne
            let SE = toCorrectQTree se
            let SW = toCorrectQTree sw

            match NW, NE, SE, SW with 
            | Leaf (value1, size1), 
                Leaf (value2, size2), 
                Leaf (value3, size3), 
                Leaf (value4, size4) 
                when value1 = value2 && value2 = value3 && value3 = value4 ->
                
                Leaf (value1, size1 + size2)

            | _ -> Node (NW, NE, SE, SW) 

    let rec private fromCorrectQTree qtree =
        match qtree with 
        | Leaf (value, 1) ->
            Leaf (value, 1)

        | Leaf (value, size) when size > 1 -> 
            Node (
                fromCorrectQTree (Leaf (value, size / 2)),
                fromCorrectQTree (Leaf (value, size / 2)),
                fromCorrectQTree (Leaf (value, size / 2)),
                fromCorrectQTree (Leaf (value, size / 2))
            )

        | Node (nw, ne, se, sw) -> 
            Node (
                fromCorrectQTree nw,
                fromCorrectQTree ne,
                fromCorrectQTree se,
                fromCorrectQTree sw
            )

        | Leaf(_, _) -> failwith "Not Implemented"

 
    let rec map func qtree = 
        match qtree with  
        | Leaf (value, size) -> Leaf (func value, size) 
        | Node (nw,ne, se, sw) -> 
            Node (map func nw, map func ne, map func se, map func sw) 
        |> toCorrectQTree
        
     
    let rec map2 func qtree1 qtree2 = 
        match qtree1, qtree2 with  
        | Leaf (value1, size1), Leaf (value2, size2) ->
            Leaf (func value1 value2, min size1 size2) 

        | Leaf (value, size), Node (nw, ne, se, sw) -> 
            Node ( 
                map2 func (Leaf (value, size)) nw, 
                map2 func (Leaf (value, size)) ne, 
                map2 func (Leaf (value, size)) se, 
                map2 func (Leaf (value, size)) sw  
            ) 
 
        | Node (nw, ne, se, sw), Leaf (value, size) -> 
            Node ( 
                map2 func nw (Leaf (value, size)), 
                map2 func ne (Leaf (value, size)), 
                map2 func se (Leaf (value, size)), 
                map2 func sw (Leaf (value, size)) 
            ) 
 
        | Node (nw, ne, se, sw), Node (NW, NE, SE, SW)  -> 
            Node ( 
                map2 func nw NW, 
                map2 func ne NE, 
                map2 func se SE, 
                map2 func sw SW 
            ) 
        |> toCorrectQTree

    let rec private add qtree1 qtree2 (opAdd: 't -> 't -> 't) (opMult: 't -> 't -> 't) =
        match qtree1, qtree2 with
        | Leaf (value1, size1), Leaf (value2, size2) -> 
            Leaf (opAdd value1 value2, size1)

        | Leaf (value, size), Node (nw, ne, se, sw) -> 
            Node ( 
                add (Leaf (value, size)) nw opAdd opMult,
                add (Leaf (value, size)) ne opAdd opMult,
                add (Leaf (value, size)) se opAdd opMult,
                add (Leaf (value, size)) sw opAdd opMult
            ) 
 
        | Node (nw, ne, se, sw), Leaf (value, size) -> 
            Node ( 
                add nw (Leaf (value, size)) opAdd opMult,
                add ne (Leaf (value, size)) opAdd opMult,
                add se (Leaf (value, size)) opAdd opMult,
                add sw (Leaf (value, size)) opAdd opMult 
            ) 

        | Node (nw, ne, se, sw), Node (NW, NE, SE, SW)  ->
            Node ( 
                add nw NW opAdd opMult,
                add ne NE opAdd opMult,
                add se SE opAdd opMult,
                add sw SW opAdd opMult 
            ) 

    let rec private mult qtree1 qtree2 (opAdd: 't -> 't -> 't) (opMult: 't -> 't -> 't) =
        match qtree1, qtree2 with
            | Leaf (value1, size1), Leaf (value2, size2) -> 
                Leaf(opMult value1 value2, size1 )

            | Leaf (value, size), Node (nw, ne, se, sw) -> 
                let Vnw = mult (Leaf (value, size)) nw opAdd opMult
                let Vse = mult (Leaf (value, size)) se opAdd opMult
                let Vne = mult (Leaf (value, size)) ne opAdd opMult
                let Vsw = mult (Leaf (value, size)) sw opAdd opMult
            
                Node (
                    add Vnw Vsw opAdd opMult, 
                    add Vne Vse opAdd opMult, 
                    add Vne Vse opAdd opMult, 
                    add Vnw Vsw opAdd opMult
                )

            | Node (nw, ne, se, sw), Leaf (value, size) -> 
                let nwV = mult nw (Leaf (value, size)) opAdd opMult
                let neV = mult ne (Leaf (value, size)) opAdd opMult
                let seV = mult se (Leaf (value, size)) opAdd opMult
                let swV = mult sw (Leaf (value, size)) opAdd opMult

                Node (
                    add nwV neV opAdd opMult, 
                    add nwV neV opAdd opMult, 
                    add swV seV opAdd opMult, 
                    add swV seV opAdd opMult
                )

            | Node (nw, ne, se, sw), Node (NW, NE, SE, SW)  ->
                let nwNW = mult nw NW opAdd opMult
                let neSE = mult ne SE opAdd opMult
                let nwNE = mult nw NE opAdd opMult
                let neSW = mult ne SW opAdd opMult
                let swNE = mult sw NE opAdd opMult
                let seSE = mult se SE opAdd opMult
                let swNW = mult sw NW opAdd opMult
                let seSW = mult se SW opAdd opMult
            
                Node (
                    add nwNW neSW opAdd opMult, 
                    add nwNE neSE opAdd opMult, 
                    add swNE seSE opAdd opMult, 
                    add swNW seSW opAdd opMult
                )
        
    let multiply qtree1 qtree2 (opAdd: 't -> 't -> 't) (opMult: 't -> 't -> 't) =
        mult (qtree1 |> fromCorrectQTree) (qtree2 |> fromCorrectQTree) opAdd opMult
        |> toCorrectQTree 

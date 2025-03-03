namespace QTrees 

 
type QTree<'t> =  
    | Leaf of 't 
    | Node of QTree<'t> * QTree<'t> * QTree<'t> * QTree<'t> 


type Matrix<'t> = 
    {
        n: int; 
        qtree: QTree<'t> 
    }

     
module QTrees =  

    let rec private toCorrectQTree qtree =
        match qtree with 
        | Leaf value -> Leaf value
        
        | Node (nw, ne, se, sw) ->
            let NW = toCorrectQTree nw
            let NE = toCorrectQTree ne
            let SE = toCorrectQTree se
            let SW = toCorrectQTree sw

            match NW, NE, SE, SW with 
            |   Leaf value1, 
                Leaf value2, 
                Leaf value3, 
                Leaf value4 
                when value1 = value2 && value2 = value3 && value3 = value4 ->
                
                Leaf value1

            | _ -> Node (NW, NE, SE, SW) 

    let rec map func qtree = 
        match qtree with  
        | Leaf value -> Leaf (func value)
        | Node (nw,ne, se, sw) -> 
            Node (map func nw, map func ne, map func se, map func sw) 
        |> toCorrectQTree
        
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
        |> toCorrectQTree

    let rec private add qtree1 qtree2 size (opAdd: 't -> 't -> 't) (opMult: 't -> 't -> 't) =
        match qtree1, qtree2 with
        | Leaf value1, Leaf value2 -> 
            Leaf(opAdd value1 value2)

        | Leaf _ , Node(nw, ne, sw, se) ->
            Node(
                add qtree1 nw (size / 2) opAdd opMult,
                add qtree1 ne (size / 2) opAdd opMult,
                add qtree1 sw (size / 2) opAdd opMult,
                add qtree1 se (size / 2) opAdd opMult
            )

        | Node(nw, ne, sw, se), Leaf _ ->
            Node(
                add nw qtree2 (size / 2) opAdd opMult,
                add ne qtree2 (size / 2) opAdd opMult,
                add sw qtree2 (size / 2) opAdd opMult,
                add se qtree2 (size / 2) opAdd opMult
            )

        | Node(nw, ne, sw, se), Node(NW, NE, SW, SE) ->
            Node(
                add nw NW (size / 2) opAdd opMult,
                add ne NE (size / 2) opAdd opMult,
                add sw SW (size / 2) opAdd opMult,
                add se SE (size / 2) opAdd opMult
            )

    let rec private mult qtree1 qtree2 size (opAdd: 't -> 't -> 't) (opMult: 't -> 't -> 't) =
        match qtree1, qtree2 with
        | Leaf value1, Leaf value2 -> 
            let add = opMult value1 value2
            let mutable value = Unchecked.defaultof<'t>

            for i in 0 .. size - 1 do
                value <- opAdd value add

            Leaf value

        | Leaf _ , Node(nw, ne, sw, se) ->
            let Vnw = mult qtree1 nw (size / 2) opAdd opMult
            let Vsw = mult qtree1 sw (size / 2) opAdd opMult
            let Vne = mult qtree1 ne (size / 2) opAdd opMult
            let Vse = mult qtree1 se (size / 2) opAdd opMult

            Node(
                add Vnw Vsw (size / 2) opAdd opMult,
                add Vne Vse (size / 2) opAdd opMult,
                add Vnw Vsw (size / 2) opAdd opMult,
                add Vne Vse (size / 2) opAdd opMult
            )

        | Node(nw, ne, sw, se), Leaf _ ->
            let nwV = mult nw qtree2 (size / 2) opAdd opMult
            let neV = mult ne qtree2 (size / 2) opAdd opMult
            let swV = mult sw qtree2 (size / 2) opAdd opMult
            let seV = mult se qtree2 (size / 2) opAdd opMult

            Node(
                add nwV neV (size / 2) opAdd opMult,
                add nwV neV (size / 2) opAdd opMult,
                add swV seV (size / 2) opAdd opMult,
                add swV seV (size / 2) opAdd opMult
            )

        | Node(nw, ne, sw, se), Node(NW, NE, SW, SE) ->
            let nwNW = mult nw NW (size / 2) opAdd opMult
            let neSW = mult ne SW (size / 2) opAdd opMult
            let nwNE = mult nw NE (size / 2) opAdd opMult
            let neSE = mult ne SE (size / 2) opAdd opMult
            let swNW = mult sw NW (size / 2) opAdd opMult
            let seSW = mult se SW (size / 2) opAdd opMult
            let swNE = mult sw NE (size / 2) opAdd opMult
            let seSE = mult se SE (size / 2) opAdd opMult

            Node(
                add nwNW neSW (size / 2) opAdd opMult,
                add nwNE neSE (size / 2) opAdd opMult,
                add swNW seSW (size / 2) opAdd opMult,
                add swNE seSE (size / 2) opAdd opMult
            )

    ///It is assumed that the matrices have already been converted to the same correct size: nxn where n = 2^N
    let multiply (matr1: Matrix<'t>) (matr2: Matrix<'t>) (opAdd: 't -> 't -> 't) (opMult: 't -> 't -> 't) =
        let result = mult matr1.qtree matr2.qtree matr1.n opAdd opMult
        let matrix = result |> toCorrectQTree
        { n = max matr1.n matr2.n; qtree = matrix }

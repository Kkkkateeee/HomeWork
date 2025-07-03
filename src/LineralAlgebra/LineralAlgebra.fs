namespace LineralAlgebra

 
type QTree<'t> =  
    | Leaf of 't 
    | Node of QTree<'t> * QTree<'t> * QTree<'t> * QTree<'t> 


type Matrix<'t> = 
    {
        n: int; 
        qtree: QTree<'t> 
    }


module private Private = 

    let rec toCorrectQTree qtree =
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


open Private


module QTrees =  
    let toCorrectQTree qtree =
        qtree |> toCorrectQTree
    
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

    /// <summary>Creates a square array2D with elements from a quadtree.</summary>
    /// <param name="qtree">The quadtree from which the elements are taken.</param>
    /// <param name="size">The output array size can only be a power of two due to the structure of quadtrees.</param>
    /// <returns>The created array2D.</returns>
    let qtreeToArray2D (qtree: QTree<'t>) (size: int) : 't[,] =
        if size <= 0 || size &&& (size - 1) <> 0 then 
            failwithf "\n\n The size is not a power of two or <= 0\n\n size: %d\n" size

        let rec qtrToArray2D (qtree: QTree<'t>) (size: int) : 't[,] = 
            match qtree with
            | Leaf value when size = 1 ->
                Array2D.create 1 1 value

            | Leaf value when size > 1 ->
                Array2D.create size size value

            | Node (nw, ne, sw, se) ->
                let NW = qtrToArray2D nw (size / 2)
                let NE = qtrToArray2D ne (size / 2)
                let SW = qtrToArray2D sw (size / 2)
                let SE = qtrToArray2D se (size / 2)

                let mainSize = Array2D.length1 NW

                let matrix = Array2D.create (mainSize * 2) (mainSize * 2) Unchecked.defaultof<'t>

                for i in 0 .. mainSize - 1 do
                    for j in 0 .. mainSize - 1 do
                        matrix[i, j] <- NW[i, j]
                        matrix[i, j + mainSize] <- NE[i, j]
                        matrix[i + mainSize, j] <- SW[i, j]
                        matrix[i + mainSize, j + mainSize] <- SE[i, j]

                matrix

            | Leaf _ -> failwith "Not Implemented"

        qtrToArray2D qtree size

    let rec height qtree = 
        match qtree with  
            | Leaf _-> 1 
            | Node (nw, ne, se, sw) -> 
                let heights = [| height nw; height ne; height se; height sw |] 
                Array.max heights + 1 


module Matrix =   

    let map func (matr: Matrix<'t>) = 
        let res = QTrees.map func matr.qtree
        { n = matr.n; qtree = res } 

    let map2 func (matr1: Matrix<'t>) (matr2: Matrix<'t>) =
        let res = QTrees.map2 func matr1.qtree matr2.qtree
        { n = max matr1.n matr2.n; qtree = res } 

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

    ///The function assumes that the matrices have already been converted to the same correct size: nxn where n = 2^N
    let multiply (matr1: Matrix<'t>) (matr2: Matrix<'t>) (opAdd: 't -> 't -> 't) (opMult: 't -> 't -> 't) =
        if matr1.n <> matr2.n then 
            failwithf "\n\n Matr1 and matr2 have different sizes:\n\n Matr1: %d\n Matr2: %d\n" matr1.n matr2.n
        elif matr1.n <= 0 || matr1.n &&& (matr1.n - 1) <> 0 then 
            failwithf "\n\n The size of matr1 is not a power of two or <= 0\n\n Matr1: %d\n" matr1.n
        elif matr2.n <= 0 || matr2.n &&& (matr2.n - 1) <> 0 then 
            failwithf "\n\n The size of matr2 is not a power of two or <= 0\n\n Matr1: %d\n" matr2.n

        let result = mult matr1.qtree matr2.qtree matr1.n opAdd opMult
        let matrix = result |> toCorrectQTree
        { n = max matr1.n matr2.n; qtree = matrix }
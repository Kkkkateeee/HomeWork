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
    /// <summary>The function goes through the whole tree and merges (if any) 4 leaves with the same values, coming from one node into one leaf of the original node with the original value.</summary>
    /// <param name="qtree">The input qtree.</param>
    /// <returns>Correctly built qtree.</returns>
    let toCorrectQTree qtree =
        qtree |> toCorrectQTree
    
    /// <summary>Builds a new qtree whose elements are the results of applying the given function to each of the elements of the qtree.</summary>
    /// <param name="mapping">The function to transform elements of the qtree.</param>
    /// <param name="qtree">The input qtree.</param>
    /// <returns>The qtree of transformed elements.</returns>
    let rec map mapping qtree = 
        match qtree with  
        | Leaf value -> Leaf (mapping value)
        | Node (nw,ne, se, sw) -> 
            Node (map mapping nw, map mapping ne, map mapping se, map mapping sw) 
        |> toCorrectQTree
        
    /// <summary>Builds a new qtree whose elements are the results of applying the given function to the corresponding elements of the two collections pairwise.</summary>
    /// <param name="mapping">The function to transform the pairs of the input elements.</param>
    /// <param name="qtree1">The first input qtree.</param>
    /// <param name="qtree2">The second input qtree.</param>
    /// <returns>The qtree of transformed elements.</returns>
    let rec map2 mapping qtree1 qtree2 = 
        match qtree1, qtree2 with  
        | Leaf value1, Leaf value2 ->
            Leaf (mapping value1 value2) 

        | Leaf value, Node (nw, ne, se, sw) -> 
            Node ( 
                map2 mapping (Leaf value) nw, 
                map2 mapping (Leaf value) ne, 
                map2 mapping (Leaf value) se, 
                map2 mapping (Leaf value) sw  
            ) 
 
        | Node (nw, ne, se, sw), Leaf value -> 
            Node ( 
                map2 mapping nw (Leaf value), 
                map2 mapping ne (Leaf value), 
                map2 mapping se (Leaf value), 
                map2 mapping sw (Leaf value) 
            ) 
 
        | Node (nw, ne, se, sw), Node (NW, NE, SE, SW)  -> 
            Node ( 
                map2 mapping nw NW, 
                map2 mapping ne NE, 
                map2 mapping se SE, 
                map2 mapping sw SW 
            ) 
        |> toCorrectQTree

    /// <summary>Creates a square array2D with elements from a qtree.</summary>
    /// <param name="qtree">The qtree from which the elements are taken.</param>
    /// <param name="size">The output array size can only be a power of two due to the structure of qtrees.</param>
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

    /// <summary>Calculates the number of levels at which tree nodes are located, from the root to any leaf.</summary>
    /// <param name="qtree">The input qtree.</param>
    /// <returns>The number of levels in the qtree.</returns>
    let rec height qtree = 
        match qtree with  
            | Leaf _-> 1 
            | Node (nw, ne, se, sw) -> 
                let heights = [| height nw; height ne; height se; height sw |] 
                Array.max heights + 1 


module Matrix =   

    /// <summary>Builds a new matrix whose elements are the results of applying the given function to each of the elements of the matrix.</summary>
    /// <param name="mapping">The function to transform elements of the matrix.</param>
    /// <param name="matr"> The input matrix.</param>
    /// <returns>The matrix of transformed elements.</returns>
    let map mapping (matr: Matrix<'t>) = 
        let res = QTrees.map mapping matr.qtree
        { n = matr.n; qtree = res } 

    /// <summary>Builds a new matrix whose elements are the results of applying the given function to the corresponding elements of the two collections pairwise.</summary>
    /// <param name="mapping">The function to transform the pairs of the input elements.</param>
    /// <param name="matr1">The first input matrix.</param>
    /// <param name="matr2">The second input matrix.</param>
    /// <returns>The matrix of transformed elements.</returns>
    let map2 mapping (matr1: Matrix<'t>) (matr2: Matrix<'t>) =
        let res = QTrees.map2 mapping matr1.qtree matr2.qtree
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

    let rec private mult qtree1 qtree2 size (opAdd: 't -> 't -> 't) (opMult: 't -> 't -> 't) (neutral: 't) =
        match qtree1, qtree2 with
        | Leaf value1, Leaf value2 -> 
            let add = opMult value1 value2
            let mutable value = neutral

            for i in 0 .. size - 1 do
                value <- opAdd value add

            Leaf value

        | Leaf _ , Node(nw, ne, sw, se) ->
            let Vnw = mult qtree1 nw (size / 2) opAdd opMult neutral
            let Vsw = mult qtree1 sw (size / 2) opAdd opMult neutral
            let Vne = mult qtree1 ne (size / 2) opAdd opMult neutral
            let Vse = mult qtree1 se (size / 2) opAdd opMult neutral

            Node(
                add Vnw Vsw (size / 2) opAdd opMult,
                add Vne Vse (size / 2) opAdd opMult,
                add Vnw Vsw (size / 2) opAdd opMult,
                add Vne Vse (size / 2) opAdd opMult
            )

        | Node(nw, ne, sw, se), Leaf _ ->
            let nwV = mult nw qtree2 (size / 2) opAdd opMult neutral
            let neV = mult ne qtree2 (size / 2) opAdd opMult neutral
            let swV = mult sw qtree2 (size / 2) opAdd opMult neutral
            let seV = mult se qtree2 (size / 2) opAdd opMult neutral

            Node(
                add nwV neV (size / 2) opAdd opMult,
                add nwV neV (size / 2) opAdd opMult,
                add swV seV (size / 2) opAdd opMult,
                add swV seV (size / 2) opAdd opMult
            )

        | Node(nw, ne, sw, se), Node(NW, NE, SW, SE) ->
            let nwNW = mult nw NW (size / 2) opAdd opMult neutral
            let neSW = mult ne SW (size / 2) opAdd opMult neutral
            let nwNE = mult nw NE (size / 2) opAdd opMult neutral
            let neSE = mult ne SE (size / 2) opAdd opMult neutral
            let swNW = mult sw NW (size / 2) opAdd opMult neutral
            let seSW = mult se SW (size / 2) opAdd opMult neutral
            let swNE = mult sw NE (size / 2) opAdd opMult neutral
            let seSE = mult se SE (size / 2) opAdd opMult neutral

            Node(
                add nwNW neSW (size / 2) opAdd opMult,
                add nwNE neSE (size / 2) opAdd opMult,
                add swNW seSW (size / 2) opAdd opMult,
                add swNE seSE (size / 2) opAdd opMult
            )

    /// <summary>The function assumes that the matrices have already been converted to the same correct size: nxn where n = 2^N.</summary>
    /// <param name="matr1">First matrix in matrix product.</param>
    /// <param name="matr2">Second matrix in matrix product.</param>
    /// <param name="opAdd">Operation of addition of elements 't.</param>
    /// <param name="opMult">Operation of multiplication of elements 't.</param>
    /// <param name="neutral">Neutral element by addition.</param>
    /// <returns>Product of matr1 * matr2.</returns>
    let multiply (matr1: Matrix<'t>) (matr2: Matrix<'t>) (opAdd: 't -> 't -> 't) (opMult: 't -> 't -> 't) (neutral: 't) =
        if matr1.n <> matr2.n then 
            failwithf "\n\n Matr1 and matr2 have different sizes:\n\n Matr1: %d\n Matr2: %d\n" matr1.n matr2.n
        elif matr1.n <= 0 || matr1.n &&& (matr1.n - 1) <> 0 then 
            failwithf "\n\n The size of matr1 is not a power of two or <= 0\n\n Matr1: %d\n" matr1.n
        elif matr2.n <= 0 || matr2.n &&& (matr2.n - 1) <> 0 then 
            failwithf "\n\n The size of matr2 is not a power of two or <= 0\n\n Matr1: %d\n" matr2.n

        let result = mult matr1.qtree matr2.qtree matr1.n opAdd opMult neutral
        let matrix = result |> toCorrectQTree
        { n = max matr1.n matr2.n; qtree = matrix }
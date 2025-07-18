namespace DataAndFuncs

open System 
open System.Collections.Generic
open Xunit 
open FsCheck 
open FsCheck.Xunit 
open FsCheck.FSharp
open LineralAlgebra

open Graphs


module Data = 
    let leaf = Leaf (Some [|1|])
    let gLeaf1 = { n = 1; qtree = leaf }
    let gLeaf4 = { n = 4; qtree = leaf }

    let leafArr = Leaf (Some [|1; 2; 3|])
    let gLeafArr1 = { n = 1; qtree = leafArr }
    let gLeafArr4 = { n = 4; qtree = leafArr }

    let node = Node ( 
        Node ( Leaf None, Leaf (Some [|1|]), Leaf (Some [|1; 2|]), Leaf (Some [|3|])), 
        Node ( Leaf (Some [|4; 5|]), Leaf (Some [|6|]), Leaf (Some [|7; 8|]), Leaf (Some [|9|])), 
        Leaf (Some [|10|]),  
        Node ( Leaf (Some [|11|]), Leaf (Some [|12|]), Leaf (Some [|13|]), Leaf (Some [|14|]))  
        ) 
    let gNode = { n = 4; qtree = node }

    let zeroGraph = { n = 4; qtree = Leaf None }
    let fullGraph = { n = 4; qtree = Leaf (Some [|1|])}
    let fullGraph2 = { n = 4; qtree = Leaf (Some [|2|])}
    
    let node1 = Node ( 
        Leaf None, 
        Leaf (Some [|'a'|]), 
        Leaf None, 
        Leaf None 
        ) 
    let graph1 = { n = 2; qtree = node1 }
    let node2 = Node ( 
        Node ( Leaf None, Leaf (Some [|true|]), Leaf None, Leaf None), 
        Node ( Leaf None, Leaf None, Leaf (Some [|true|]), Leaf None), 
        Leaf None,  
        Node ( Leaf None, Leaf (Some [|true|]), Leaf None, Leaf None)  
        )  
    let graph2 = { n = 4; qtree = node2 }

    let directed = Node ( 
        Node ( Leaf None, Leaf (Some [|1|]), Leaf None, Leaf None), 
        Node ( Leaf None, Leaf None, Leaf (Some [|1|]), Leaf None),
        Leaf None,
        Node ( Leaf None, Leaf (Some [|1|]), Leaf None, Leaf None)
    )  
    let dirGraph = { n = 4; qtree = directed }
    let directed2 = Node ( 
        Node ( Leaf None, Leaf (Some [|2|]), Leaf None, Leaf None), 
        Node ( Leaf None, Leaf None, Leaf (Some [|2|]), Leaf None),
        Leaf None,
        Node ( Leaf None, Leaf (Some [|2|]), Leaf None, Leaf None)
    )  
    let dirGraph2 = { n = 4; qtree = directed2 }
    let undirected = Node ( 
        Node ( Leaf None, Leaf (Some [|1|]), Leaf (Some [|1|]), Leaf None), 
        Node ( Leaf None, Leaf None, Leaf (Some [|1|]), Leaf None),
        Node ( Leaf None, Leaf (Some [|1|]), Leaf None, Leaf None),
        Node ( Leaf None, Leaf (Some [|1|]), Leaf (Some [|1|]), Leaf None)
        )  
    let undirGraph = { n = 4; qtree = undirected }
    let undirected2 = Node ( 
        Node ( Leaf None, Leaf (Some [|2|]), Leaf (Some [|2|]), Leaf None), 
        Node ( Leaf None, Leaf None, Leaf (Some [|2|]), Leaf None),
        Node ( Leaf None, Leaf (Some [|2|]), Leaf None, Leaf None),
        Node ( Leaf None, Leaf (Some [|2|]), Leaf (Some [|2|]), Leaf None)
        )  
    let undirGraph2 = { n = 4; qtree = undirected2 }


module Generator = 
    let GraphGen size typeOfMatr = 
        let LeafGen = 
            let someOrNone = Random().Next(0, 1)
            if someOrNone = 0 then Leaf None
            else
                let size = Random().Next(1, 10)
                let array = Array.zeroCreate size
                for i in 0 .. size - 1 do
                    array.[i] <- Random().Next(-100, 10)
                
                Leaf (Some array)

        let rec QTreeGen size typeOfMatr =
            if size = 1 then 
                match typeOfMatr with
                | "Leaf" -> LeafGen
                | "Node" -> LeafGen
                | _ -> failwith "Not Implemented"
            else
                let nw = QTreeGen (size / 2) typeOfMatr
                let ne = QTreeGen (size / 2) typeOfMatr
                let sw = QTreeGen (size / 2) typeOfMatr
                let se = QTreeGen (size / 2) typeOfMatr
                Node (nw, ne, sw, se)

        let MatrGen (size: int) =
            let qtree = QTreeGen size typeOfMatr
            { n = size; qtree = QTrees.toCorrectQTree qtree }

        let res = MatrGen size
        res


module Funcs = 
    let combinations k (array: 'T[]) =
        let rec combine k (currentIndex : int) (currentCombination : 'T list) = seq {
            if currentCombination.Length = k then
                yield List.toArray currentCombination 
            else
                for i = currentIndex to array.Length - 1 do
                    yield! combine k (i + 1) (array.[i] :: currentCombination)  
        }
        combine k 0 [] 
        |> Seq.toArray

    let toAdjacencyMatrix (graph: Matrix<Edjes<'t>>) =
        let func (value: Edjes<'t>) = 
            match value with 
            | Some _ -> 1
            | None -> 0
        let res = Matrix.map func graph
        res

    let getRowOfMatrix (matr: Matrix<'t>) (i: int) =
        let n = matr.n 

        let rec getElement (qtree: QTree<'t>) (size: int) (row: int) (col: int) =
            match qtree with
            | Leaf value -> value
            | Node(nw, ne, sw, se) ->
                let halfSize = size / 2
                if row < halfSize then 
                    if col < halfSize then 
                        getElement nw halfSize row col
                    else 
                        getElement ne halfSize row (col - halfSize)
                else 
                    if col < halfSize then 
                        getElement sw halfSize (row - halfSize) col
                    else 
                        getElement se halfSize (row - halfSize) (col - halfSize)

        let res = Array.zeroCreate n
        for col in 0 .. n - 1 do
            res.[col] <- getElement matr.qtree n i col 
        res

    let toWeightedAdjacencyMatrix (garph: Matrix<Edjes<'t>>) (minFunc: array<'t> -> 't) (inf: 't) = 
        let rec buildQTree (qtree: QTree<Edjes<'t>>) (row: int, col: int, size: int) =
            match qtree with 
            | Node(nw, ne, sw, se) ->
                    let halfSize = size / 2
                    let topLeft = buildQTree nw (row, col, halfSize)
                    let topRight = buildQTree ne (row, col + halfSize, halfSize)
                    let bottomLeft = buildQTree sw (row + halfSize, col, halfSize)
                    let bottomRight = buildQTree se (row + halfSize, col + halfSize, halfSize)
                    Node (topLeft, topRight, bottomLeft, bottomRight)

            | Leaf edjes when size > 1 ->
                    let halfSize = size / 2
                    let topLeft = buildQTree (Leaf edjes) (row, col, halfSize)
                    let topRight = buildQTree (Leaf edjes) (row, col + halfSize, halfSize)
                    let bottomLeft = buildQTree (Leaf edjes) (row + halfSize, col, halfSize)
                    let bottomRight = buildQTree (Leaf edjes) (row + halfSize, col + halfSize, halfSize)
                    Node (topLeft, topRight, bottomLeft, bottomRight)

            | Leaf edjes when size = 1 ->
                if row = col then
                    Leaf Unchecked.defaultof<'t>
                else
                    match edjes with 
                        | Some arr -> Leaf (minFunc arr)
                        | None -> Leaf inf

            | Leaf _ -> failwith "Not Implemented"

        { n = garph.n; qtree = buildQTree garph.qtree (0, 0, garph.n) }


    let rec getElOfQTree (qtree: QTree<Edjes<int>>) = 
        let handleLeaf edjes =
            match edjes with 
            | None -> 
                Unchecked.defaultof<'t>
            | Some array -> 
                Array.min array

        match qtree with
        | Leaf edjes -> handleLeaf edjes
        | Node(_, _, _, _) -> failwith "Not Implemented"
        
    let rec findValue (qtree: QTree<'t>) (size: int) i j : 't =
        match qtree with
        | Leaf value -> value
        | Node(nw, ne, sw, se) ->
            if i < size / 2 && j < size / 2 then
                findValue nw (size / 2) i j
            elif i < size / 2 && j >= size / 2 then
                findValue ne (size / 2) i (j - size / 2)
            elif i >= size / 2 && j < size / 2 then
                findValue sw (size / 2) (i - size / 2) j
            else
                findValue se (size / 2) (i - size / 2) (j - size / 2)

    let transitiveClosureMatrices (matrix: int array2d) =
        if matrix = null then
            Array2D.create 0 0 0 
        else
            let rows = Array2D.length1 matrix
            let res = Array2D.copy matrix

            for k in 0 .. rows - 1 do  
                for i in 0 .. rows - 1 do
                    for j in 0 .. rows - 1 do
                        if res.[i,k] = 1 && res.[k,j] = 1 then
                            res.[i,j] <- 1
                        else
                            res.[i,j] <- max res.[i,j] res.[i,k] * res.[k,j]  
            res

    let origBfs (graph: Matrix<Edjes<int>>) (vertexArray: int array) : int array =
        let matr = toAdjacencyMatrix graph
        let arr2d = QTrees.qtreeToArray2D matr.qtree graph.n
        let adjM = Array.zeroCreate graph.n

        for i in 0 .. graph.n - 1 do 
            let arr = Array.zeroCreate graph.n
            for j in 0 .. graph.n - 1 do 
                arr.[j] <- arr2d.[i, j]

            adjM.[i] <- arr

        let n = adjM.Length
        let dist = Array.create n -1 
        let queue = Queue<int>()

        for vertex in vertexArray do 
            dist.[vertex] <- 0
            queue.Enqueue vertex

        while queue.Count > 0 do
            let node = queue.Dequeue()

            for j in 0 .. n - 1 do
                if adjM.[node].[j] = 1 && dist.[j] = -1 then 
                    dist.[j] <- dist.[node] + 1 
                    queue.Enqueue j
        dist 

    let origReach (graph: Matrix<Edjes<int>>) (i: int) (j: int) =
        let res = origBfs graph [|i|]
        if res.[j] = -1 then false
        else true

    let origParent (graph: Matrix<Edjes<int>>) (i: int) = 
        let dist = origBfs graph [|i|]
        let parents = Array.create graph.n -1
        
        for k in 0 .. graph.n - 1 do
            if k <> i then 
                if dist.[k] >= 0 then 
                    for j in 0 .. graph.n - 1 do 
                        if findValue (toAdjacencyMatrix graph).qtree graph.n j k = 1 && dist.[j] = dist.[k] - 1 then 
                            parents.[k] <- j
        parents

    let origSSSP (graph: Matrix<Edjes<int>>) (vertexArray: int array) =
        let opAdd1 x y =
            if x = -1 && y = -1 then -1
            elif x = -1 && y <> -1 then y
            elif y = -1 && x <> -1 then x
            else Array.min [|x; y|]

        let opMult1 x y =
            if x = -1 || y = -1 then -1
            else x + y

        let mult (matr1: int[,]) (matr2: int[,]) =
            let result = Array2D.create graph.n graph.n -1

            for i in 0 .. graph.n - 1 do
                for j in 0 .. graph.n - 1 do
                    for k in 0 .. graph.n - 1 do
                        result.[i, j] <- opAdd1 result.[i, j] (opMult1 matr1.[i, k] matr2.[k, j])
            result
                
        let matr = toWeightedAdjacencyMatrix graph Array.min -1
        let adgM = QTrees.qtreeToArray2D matr.qtree graph.n

        let mutable power = adgM

        for i in 0 .. graph.n - 1 do 
            power <- mult power adgM

        let res = [| for i in vertexArray do power.[i,*] |]

        let mutable res2 = Array.create graph.n -1
        for i in 0 .. res.Length - 1 do 
            res2 <- Array.map2 opAdd1 res2 res.[i]
        res2
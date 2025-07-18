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
    let toAdjacencyMatrix (graph: Matrix<Edjes<'t>>) =
        let func (value: Edjes<'t>) = 
            match value with 
            | Some _ -> 1
            | None -> 0
        let res = Matrix.map func graph
        res

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
namespace PropertyGraphs
 
open System 
open Xunit 
open FsCheck 
open FsCheck.Xunit 
open FsCheck.FSharp
open LineralAlgebra
open LineralAlgebra.QTrees

open Graphs
open Graphs.Graphs
open PropertyLA.DataAndFuncs


module DataAndFuncs = 

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
            { n = size; qtree = toCorrectQTree qtree }

        let res = MatrGen size
        res


open DataAndFuncs

[<Properties(MaxTest = 100)>] 
type ShortestWay() = 
    
    let power = Random().Next(1, 6)
    let size = pown 2 power

    [<Property>]
    member _.leaf1 () =
        let graph = GraphGen 1 "Leaf"
        let res = shortestWay graph 1 1 Array.min
        let ex = getElOfQTree graph.qtree
        Assert.Equal(ex, res)

    [<Property>]
    member _.leafSize () =
        let graph = GraphGen size "Leaf"
        let res = shortestWay graph 1 2 Array.min
        let ex = getElOfQTree graph.qtree
        Assert.Equal(ex, res)

    [<Property>]
    member _.node () =
        let graph = GraphGen size "Node"
        let res = Array2D.zeroCreate<int> size size
        for i in 0 .. size - 1 do 
            for j in 0 .. size - 1 do
                res.[i, j] <- shortestWay graph (i + 1) (j + 1) Array.min
    
        let graphArray2D = qtreeToArray2D graph.qtree graph.n

        let ex = Array2D.zeroCreate size size
        for i in 0 .. size - 1 do  
            for j in 0 .. size - 1 do  
                match graphArray2D.[i, j] with 
                | Some array -> ex.[i, j] <- Array.min array
                | None ->  ex.[i, j] <- 0              
        Assert.Equal(ex, res)


// [<Properties(MaxTest = 100)>] 
// type TransitiveClosure() = 
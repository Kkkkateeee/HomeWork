namespace PropertyGraphs
 
open System 
open Xunit 
open FsCheck 
open FsCheck.Xunit 
open FsCheck.FSharp
open LineralAlgebra

open Graphs
open Graphs.Graphs


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
            { n = size; qtree = QTrees.toCorrectQTree qtree }

        let res = MatrGen size
        res

    let transitiveClosureMatrices (matrix: int array2d) =
        if matrix = null then
            Array2D.create 0 0 0 
        else
            let rows = Array2D.length1 matrix
            let res = Array2D.copy matrix

            for k in 0 .. rows - 1 do  // Использовать rows или cols
                for i in 0 .. rows - 1 do
                    for j in 0 .. rows - 1 do
                        if res.[i,k] = 1 && res.[k,j] = 1 then
                            res.[i,j] <- 1
                        else
                            res.[i,j] <- max res.[i,j] res.[i,k] * res.[k,j]  
                        
            res

    let toAdjacencyMatrix (graph: Matrix<Edjes<'t>>) =
        let func (value: Edjes<'t>) = 
            match value with 
            | Some _ -> 1
            | None -> 0
        let res = Matrix.map func graph
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
    
        let graphArray2D = QTrees.qtreeToArray2D graph.qtree graph.n

        let ex = Array2D.zeroCreate size size
        for i in 0 .. size - 1 do  
            for j in 0 .. size - 1 do  
                match graphArray2D.[i, j] with 
                | Some array -> ex.[i, j] <- Array.min array
                | None ->  ex.[i, j] <- 0              
        Assert.Equal(ex, res)


[<Properties(MaxTest = 100)>] 
type TransitiveClosure() = 
    let power = Random().Next(1, 6)
    let size = pown 2 power

    [<Property>]
    member _.trClosure () =
        let generatedGraph = GraphGen size "Node"
        let generatedGraphTrCl = transitiveClosure generatedGraph
        let resArray = QTrees.qtreeToArray2D generatedGraphTrCl.qtree generatedGraph.n 

        let generatedGraphToAdjM = toAdjacencyMatrix generatedGraph
        let adjMto2Darray = QTrees.qtreeToArray2D generatedGraphToAdjM.qtree generatedGraphToAdjM.n 

        let arrayTrCl = transitiveClosureMatrices adjMto2Darray
        Assert.Equal(arrayTrCl, resArray)
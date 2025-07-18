namespace PropertyGraphs
 
open System 
open Xunit 
open FsCheck 
open FsCheck.Xunit 
open FsCheck.FSharp
open LineralAlgebra

open Graphs
open Graphs.Graphs
open DataAndFuncs.Generator
open DataAndFuncs.Funcs


[<Properties(MaxTest = 100)>] 
type ShortestWay() = 
    
    let power = Random().Next(1, 3)
    let size = pown 2 power

    [<Property>]
    member _.leaf1 () =
        let graph = GraphGen 1 "Leaf"
        let res = shortestWay graph 0 0 Array.min
        let ex = getElOfQTree graph.qtree
        Assert.Equal(ex, res)

    [<Property>]
    member _.leafSize () =
        let graph = GraphGen size "Leaf"
        let res = shortestWay graph 0 1 Array.min
        let ex = getElOfQTree graph.qtree
        Assert.Equal(ex, res)

    [<Property>]
    member _.node () =
        let graph = GraphGen size "Node"
        let res = Array2D.zeroCreate<int> size size
        for i in 0 .. size - 1 do 
            for j in 0 .. size - 1 do
                res.[i, j] <- shortestWay graph i j Array.min
    
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
    let power = Random().Next(1, 3)
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
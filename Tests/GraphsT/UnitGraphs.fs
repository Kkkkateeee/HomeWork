namespace UnitGraphs
 
open System 
open Xunit 
open FsCheck 
open FsCheck.Xunit 
open FsCheck.FSharp
open LineralAlgebra

open Graphs.Graphs
open DataAndFuncs.Data


module ShortestWay =
    
    [<Fact>]
    let leaf () =
        let res = shortestWay gLeaf1 0 0 Array.min
        let ex = 1
        Assert.Equal(ex, res)

    [<Fact>]
    let leaf4 () =
        let res = shortestWay gLeaf4 0 1 Array.min
        let ex = 1
        Assert.Equal(ex, res)

    [<Fact>]
    let leafArr () =
        let res = shortestWay gLeafArr1 0 0 Array.min
        let ex = 1
        Assert.Equal(ex, res)

    [<Fact>]
    let leafArr4 () =
        let res = shortestWay gLeafArr4 1 0 Array.min
        let ex = 1
        Assert.Equal(ex, res)

    [<Fact>]
    let node () =
        let res = Array2D.zeroCreate<int> 4 4
        for i in 0 .. 3 do 
            for j in 0 .. 3 do
                res.[i, j] <- shortestWay gNode i j Array.min
    
        let arrayArray =
            [|
                [|0; 1; 4; 6|];
                [|1; 3; 7; 9|];
                [|10; 10; 11; 12|];
                [|10; 10; 13; 14|]
            |]

        let ex = Array2D.init 4 4 (fun i j ->
                if j < arrayArray.[i].Length then
                    arrayArray.[i].[j]
                else
                    0 
            )
            
        Assert.Equal(ex, res)
        

module TransitiveClosure =

    [<Fact>]
    let zeroGraph () = 
        let res = transitiveClosure zeroGraph
        let ex = { n = 4; qtree = Leaf 0 }
        Assert.Equal(ex, res)

    [<Fact>]
    let fullGraph () = 
        let res = transitiveClosure fullGraph
        let ex = { n = 4; qtree = Leaf 1 }
        Assert.Equal(ex, res)

    [<Fact>]
    let graph2x2 () =
        let res = transitiveClosure graph1
        let node = Node ( 
            Leaf 0, 
            Leaf 1, 
            Leaf 0, 
            Leaf 0 
            ) 
        let ex = { n = 2; qtree = node }
        Assert.Equal(ex, res)

    [<Fact>]
    let graph4x4 () =
        let res = transitiveClosure graph2
        let node = Node ( 
            Node ( Leaf 0, Leaf 1, Leaf 0, Leaf 0), 
            Leaf 1, 
            Leaf 0,  
            Node ( Leaf 0, Leaf 1, Leaf 0, Leaf 0)  
            ) 
        let ex = { n = 4; qtree = node }
        Assert.Equal(ex, res)
namespace UnitGraphs
 
open System 
open Xunit  
open LineralAlgebra
 
open Graphs
open Graphs.Graphs
 
 
module DataAndFuncs = 
    // for ShortestWay
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

    // for TransitiveClosure
    let zeroGraph = { n = 4; qtree = Leaf None }
    let fullGraph = { n = 4; qtree = Leaf (Some [|2|])}
    
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


open DataAndFuncs


module ShortestWay =
    
    [<Fact>]
    let leaf () =
        let res = shortestWay gLeaf1 1 1 Array.min
        let ex = 1
        Assert.Equal(ex, res)

    [<Fact>]
    let leaf4 () =
        let res = shortestWay gLeaf4 1 2 Array.min
        let ex = 1
        Assert.Equal(ex, res)

    [<Fact>]
    let leafArr () =
        let res = shortestWay gLeafArr1 1 1 Array.min
        let ex = 1
        Assert.Equal(ex, res)

    [<Fact>]
    let leafArr4 () =
        let res = shortestWay gLeafArr4 2 1 Array.min
        let ex = 1
        Assert.Equal(ex, res)

    [<Fact>]
    let node () =
        let res = Array2D.zeroCreate<int> 4 4
        for i in 0 .. 3 do 
            for j in 0 .. 3 do
                res.[i, j] <- shortestWay gNode (i + 1) (j + 1) Array.min
    
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
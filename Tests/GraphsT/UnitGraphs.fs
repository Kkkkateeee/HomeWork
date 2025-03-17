namespace UnitGraphs
 
open System 
open Xunit  
open LineralAlgebra
 
open Graphs
open Graphs.Graphs
 
 
module DataAndFuncs = 

    let leaf = Leaf (Some [|1|])
    let Gleaf1 = { n = 1; qtree = leaf }
    let Gleaf4 = { n = 4; qtree = leaf }

    let leafArr = Leaf (Some [|1; 2; 3|])
    let GleafArr1 = { n = 1; qtree = leafArr }
    let GleafArr4 = { n = 4; qtree = leafArr }

    let node = Node ( 
        Node ( Leaf None, Leaf (Some [|-1|]), Leaf (Some [|1; 2|]), Leaf (Some [|3|])), 
        Node ( Leaf (Some [|4; 5|]), Leaf (Some [|6|]), Leaf (Some [|7; 8|]), Leaf (Some [|9|])), 
        Leaf (Some [|10|]),  
        Node ( Leaf (Some [|11|]), Leaf (Some [|12|]), Leaf (Some [|13|]), Leaf (Some [|14|]))  
        ) 

    let Gnode = { n = 4; qtree = node }
    let node1 = Node ( 
        Node ( Leaf 0, Leaf 1, Leaf 0, Leaf 0), 
        Node ( Leaf 0, Leaf 0, Leaf 1, Leaf 0), 
        Leaf 0,  
        Leaf 0  
        ) 
    let a = { n = 4; qtree = node1 }


open DataAndFuncs


module ShortestWay =
    
    [<Fact>]
    let leaf () =
        let res = shortestWay Gleaf1 1 1 Array.min
        let ex = 1
        Assert.Equal(ex, res)

    [<Fact>]
    let leaf4 () =
        let res = shortestWay Gleaf4 1 2 Array.min
        let ex = 1
        Assert.Equal(ex, res)

    [<Fact>]
    let leafArr () =
        let res = shortestWay GleafArr1 1 1 Array.min
        let ex = 1
        Assert.Equal(ex, res)

    [<Fact>]
    let leafArr4 () =
        let res = shortestWay GleafArr4 2 1 Array.min
        let ex = 1
        Assert.Equal(ex, res)

    [<Fact>]
    let node () =
        let res = Array2D.zeroCreate<int> 4 4
        for i in 0 .. 3 do 
            for j in 0 .. 3 do
                res.[i, j] <- shortestWay Gnode (i + 1) (j + 1) Array.min
    
        let arrayArray =
            [|
                [|0; -1; 4; 6|];
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


// module TransitiveClosure =

//     [<Fact>]
//     let node () = 
//         let res = transitiveClosure a
//         let node2 = Node ( 
//                 Node ( Leaf 0, Leaf 1, Leaf 0, Leaf 0), 
//                 Node ( Leaf 1, Leaf 0, Leaf 1, Leaf 0), 
//                 Leaf 0,  
//                 Leaf 0  
//             ) 
//         let a1 = { n = 4; qtree = node2 }
//         //let ex = { n = 4; qtree = ex1 }
//         Assert.Equal(a1, res)

(*
0 1 2 0
0 0 1 0
0 0 0 0
0 0 0 0

    let node1 = Node ( 
        Node ( Leaf 0, Leaf 1, Leaf 0, Leaf 0), 
        Node ( Leaf 0, Leaf 0, Leaf 1, Leaf 0), 
        Leaf 0,  
        Leaf 0  
        ) 
    let a = { n = 4; qtree = node1 }
*)
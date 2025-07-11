namespace UnitBFS
 
open System 
open Xunit  
open LineralAlgebra
 
open Graphs
open Graphs.Graphs
 
 
module DataAndFuncs = 
    let zeroGraph = { n = 4; qtree = Leaf None }
    let fullGraph = { n = 4; qtree = Leaf (Some [|2|])}
    let directed = Node ( 
        Node ( Leaf None, Leaf (Some [|1|]), Leaf None, Leaf None), 
        Node ( Leaf None, Leaf None, Leaf (Some [|1|]), Leaf None),
        Leaf None,
        Node ( Leaf None, Leaf (Some [|1|]), Leaf None, Leaf None)
    )  
    let dirGraph = { n = 4; qtree = directed }
    let undirected = Node ( 
        Node ( Leaf None, Leaf (Some [|1|]), Leaf (Some [|1|]), Leaf None), 
        Node ( Leaf None, Leaf None, Leaf (Some [|1|]), Leaf None),
        Node ( Leaf None, Leaf (Some [|1|]), Leaf None, Leaf None),
        Node ( Leaf None, Leaf (Some [|1|]), Leaf (Some [|1|]), Leaf None)
        )  
    let undirGraph = { n = 4; qtree = undirected }

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


open DataAndFuncs


module BFS =

    [<Fact>]
    let zeroBFS () =
        let res = Array.zeroCreate 4
        for i in 0 .. 3 do 
            res.[i] <- BFS zeroGraph i
        
        let ex = [|
            [|0; -1; -1; -1|];
            [|-1; 0; -1; -1|];
            [|-1; -1; 0; -1|];
            [|-1; -1; -1; 0|]
        |]

        for i in 0 .. 3 do 
            Assert.Equal<int>(ex.[i], res.[i])

    [<Fact>]
    let fullBFS () =
        let res = Array.zeroCreate 4
        for i in 0 .. 3 do 
            res.[i] <- BFS fullGraph i

        let ex = [|
            [|0; 1; 1; 1|];
            [|1; 0; 1; 1|];
            [|1; 1; 0; 1|];
            [|1; 1; 1; 0|]
        |]

        for i in 0 .. 3 do 
            Assert.Equal<int>(ex.[i], res.[i])

    [<Fact>]
    let dirGraph () =
        let res = Array.zeroCreate 4
        for i in 0 .. 3 do 
            res.[i] <- BFS dirGraph i

        let ex = [|
            [|0; 1; 2; 3|];
            [|-1; 0; 1; 2|];
            [|-1; -1; 0; 1|];
            [|-1; -1; -1; 0|]
        |]

        for i in 0 .. 3 do 
            Assert.Equal<int>(ex.[i], res.[i])

    [<Fact>]
    let undirGraph () =
        let res = Array.zeroCreate 4
        for i in 0 .. 3 do 
            res.[i] <- BFS undirGraph i

        let ex = [|
            [|0; 1; 2; 3|];
            [|1; 0; 1; 2|];
            [|2; 1; 0; 1|];
            [|3; 2; 1; 0|]
        |]

        for i in 0 .. 3 do 
            Assert.Equal<int>(ex.[i], res.[i])


module MultiSourceBFS =

    [<Fact>]
    let zeroBFS () =
        let res = Array.zeroCreate 15

        for i in 0 .. 3 do 
            let combination = combinations 1 [|0; 1; 2; 3|]
            res.[i] <- multiSourceBFS zeroGraph combination.[i]
        
        for i in 0 .. 5 do 
            let combination = combinations 2 [|0; 1; 2; 3|]
            res.[i + 4] <- multiSourceBFS zeroGraph combination.[i]

        for i in 0 .. 3 do 
            let combination = combinations 3 [|0; 1; 2; 3|]
            res.[i + 10] <- multiSourceBFS zeroGraph combination.[i]

        res.[14] <- multiSourceBFS zeroGraph [|0; 1; 2; 3|]

        let ex = [|
            [|0; -1; -1; -1|];
            [|-1; 0; -1; -1|];
            [|-1; -1; 0; -1|];
            [|-1; -1; -1; 0|];
            [|0; 0; -1; -1|];
            [|0; -1; 0; -1|];
            [|0; -1; -1; 0|];
            [|-1; 0; 0; -1|];
            [|-1; 0; -1; 0|];
            [|-1; -1; 0; 0|];
            [|0; 0; 0; -1|];
            [|0; 0; -1; 0|];
            [|0; -1; 0; 0|];
            [|-1; 0; 0; 0|];
            [|0; 0; 0; 0|]
        |]

        for i in 0 .. 14 do 
            Assert.Equal<int>(ex.[i], res.[i])

    [<Fact>]
    let zeroBFS_OrderOfVerticesIsntImportant () =
        let res = multiSourceBFS zeroGraph [|0; 2|]
        let res1 = multiSourceBFS zeroGraph [|2; 0|]
        
        let ex = [|0; -1; 0; -1|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let zeroBFS_RepetitionOfVerticesIsntImportant () =
        let res = multiSourceBFS zeroGraph [|2|]
        let res1 = multiSourceBFS zeroGraph [|2; 2|]
        
        let ex = [|-1; -1; 0; -1|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let fullBFS () =
        let res = Array.zeroCreate 15

        for i in 0 .. 3 do 
            let combination = combinations 1 [|0; 1; 2; 3|]
            res.[i] <- multiSourceBFS fullGraph combination.[i]
        
        for i in 0 .. 5 do 
            let combination = combinations 2 [|0; 1; 2; 3|]
            res.[i + 4] <- multiSourceBFS fullGraph combination.[i]

        for i in 0 .. 3 do 
            let combination = combinations 3 [|0; 1; 2; 3|]
            res.[i + 10] <- multiSourceBFS fullGraph combination.[i]

        res.[14] <- multiSourceBFS fullGraph [|0; 1; 2; 3|]

        let ex = [|
            [|0; 1; 1; 1|];
            [|1; 0; 1; 1|];
            [|1; 1; 0; 1|];
            [|1; 1; 1; 0|];
            [|0; 0; 1; 1|];
            [|0; 1; 0; 1|];
            [|0; 1; 1; 0|];
            [|1; 0; 0; 1|];
            [|1; 0; 1; 0|];
            [|1; 1; 0; 0|];
            [|0; 0; 0; 1|];
            [|0; 0; 1; 0|];
            [|0; 1; 0; 0|];
            [|1; 0; 0; 0|];
            [|0; 0; 0; 0|]
        |]

        for i in 0 .. 14 do 
            Assert.Equal<int>(ex.[i], res.[i])

    [<Fact>]
    let fullBFS_OrderOfVerticesIsntImportant () =
        let res = multiSourceBFS fullGraph [|0; 2|]
        let res1 = multiSourceBFS fullGraph [|2; 0|]
        
        let ex = [|0; 1; 0; 1|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let fullBFS_RepetitionOfVerticesIsntImportant () =
        let res = multiSourceBFS fullGraph [|2|]
        let res1 = multiSourceBFS fullGraph [|2; 2|]
        
        let ex = [|1; 1; 0; 1|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let dirBFS () =
        let res = Array.zeroCreate 15

        for i in 0 .. 3 do 
            let combination = combinations 1 [|0; 1; 2; 3|]
            res.[i] <- multiSourceBFS dirGraph combination.[i]
        
        for i in 0 .. 5 do 
            let combination = combinations 2 [|0; 1; 2; 3|]
            res.[i + 4] <- multiSourceBFS dirGraph combination.[i]

        for i in 0 .. 3 do 
            let combination = combinations 3 [|0; 1; 2; 3|]
            res.[i + 10] <- multiSourceBFS dirGraph combination.[i]

        res.[14] <- multiSourceBFS dirGraph [|0; 1; 2; 3|]

        let ex = [|
            [|0; 1; 2; 3|];
            [|-1; 0; 1; 2|];
            [|-1; -1; 0; 1|];
            [|-1; -1; -1; 0|];
            [|0; 0; 1; 2|];
            [|0; 1; 0; 1|];
            [|0; 1; 2; 0|];
            [|-1; 0; 0; 1|];
            [|-1; 0; 1; 0|];
            [|-1; -1; 0; 0|];
            [|0; 0; 0; 1|];
            [|0; 0; 1; 0|];
            [|0; 1; 0; 0|];
            [|-1; 0; 0; 0|];
            [|0; 0; 0; 0|]
        |]

        for i in 0 .. 14 do 
            Assert.Equal<int>(ex.[i], res.[i])

    [<Fact>]
    let dirBFS_OrderOfVerticesIsntImportant () =
        let res = multiSourceBFS dirGraph [|0; 2|]
        let res1 = multiSourceBFS dirGraph [|2; 0|]
        
        let ex = [|0; 1; 0; 1|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let dirBFS_RepetitionOfVerticesIsntImportant () =
        let res = multiSourceBFS dirGraph [|2|]
        let res1 = multiSourceBFS dirGraph [|2; 2|]
        
        let ex = [|-1; -1; 0; 1|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let undirBFS () =
        let res = Array.zeroCreate 15

        for i in 0 .. 3 do 
            let combination = combinations 1 [|0; 1; 2; 3|]
            res.[i] <- multiSourceBFS undirGraph combination.[i]
        
        for i in 0 .. 5 do 
            let combination = combinations 2 [|0; 1; 2; 3|]
            res.[i + 4] <- multiSourceBFS undirGraph combination.[i]

        for i in 0 .. 3 do 
            let combination = combinations 3 [|0; 1; 2; 3|]
            res.[i + 10] <- multiSourceBFS undirGraph combination.[i]

        res.[14] <- multiSourceBFS undirGraph [|0; 1; 2; 3|]

        let ex = [|
            [|0; 1; 2; 3|];
            [|1; 0; 1; 2|];
            [|2; 1; 0; 1|];
            [|3; 2; 1; 0|];
            [|0; 0; 1; 2|];
            [|0; 1; 0; 1|];
            [|0; 1; 1; 0|];
            [|1; 0; 0; 1|];
            [|1; 0; 1; 0|];
            [|2; 1; 0; 0|];
            [|0; 0; 0; 1|];
            [|0; 0; 1; 0|];
            [|0; 1; 0; 0|];
            [|1; 0; 0; 0|];
            [|0; 0; 0; 0|]
        |]

        for i in 0 .. 14 do 
            Assert.Equal<int>(ex.[i], res.[i])

    [<Fact>]
    let undirBFS_OrderOfVerticesIsntImportant () =
        let res = multiSourceBFS undirGraph [|0; 2|]
        let res1 = multiSourceBFS undirGraph [|2; 0|]
        
        let ex = [|0; 1; 0; 1|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let undirBFS_RepetitionOfVerticesIsntImportant () =
        let res = multiSourceBFS undirGraph [|2|]
        let res1 = multiSourceBFS undirGraph [|2; 2|]
        
        let ex = [|2; 1; 0; 1|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)


module Reachability =

    [<Fact>]
    let zeroReach () =
        for i in 0 .. 3 do 
            for j in 0 .. 3 do 
                if i <> j then 
                    let res = reachability zeroGraph i j
                    Assert.False res

    [<Fact>]
    let fullReach () =
        for i in 0 .. 3 do 
            for j in 0 .. 3 do 
                let res = reachability fullGraph i j
                Assert.True res

    [<Fact>]
    let dirReach () =
        let res = Array.zeroCreate 16
        let ex = [|
            true; true; true; true;
            false; true; true; true;
            false; false; true; true;
            false; false; false; true
        |]

        for i in 0 .. 3 do 
            for j in 0 .. 3 do 
                res.[4 * i + j] <- reachability dirGraph i j

        Assert.Equal<bool>(ex, res)

    [<Fact>]
    let undirReach () =
        for i in 0 .. 3 do 
            for j in 0 .. 3 do 
                let res = reachability undirGraph i j
                Assert.True res
                    

module Parent =
    let a = 1
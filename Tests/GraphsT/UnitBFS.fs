namespace UnitBFS
 
open System 
open Xunit 
open FsCheck 
open FsCheck.Xunit 
open FsCheck.FSharp
open LineralAlgebra
 
open Graphs.Graphs
open DataAndFuncs.Data
open DataAndFuncs.Funcs
 

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

    [<Fact>]
    let zero () =
        for i in 0 .. zeroGraph.n - 1 do 
            let res = parent zeroGraph i
            Assert.Equal<array<int>>([|-1; -1; -1; -1|], res)

    [<Fact>]
    let full () =
        let res = Array.zeroCreate 4
        for i in 0 .. fullGraph.n - 1 do
            res.[i] <- parent fullGraph i

        let ex = [|
            [|-1; 0; 0; 0|];
            [|1; -1; 1; 1|];
            [|2; 2; -1; 2|];
            [|3; 3; 3; -1|]
        |]
        Assert.Equal<array<int>>(ex, res)  

    [<Fact>]
    let dirGraph () =
        let res = Array.zeroCreate 4
        for i in 0 .. fullGraph.n - 1 do
            res.[i] <- parent dirGraph i

        let ex = [|
            [|-1; 0; 1; 2|];
            [|-1; -1; 1; 2|];
            [|-1; -1; -1; 2|];
            [|-1; -1; -1; -1|]
        |]
        Assert.Equal<array<int>>(ex, res)  

    [<Fact>]
    let undirGraph () =
        let res = Array.zeroCreate 4
        for i in 0 .. fullGraph.n - 1 do
            res.[i] <- parent undirGraph i

        let ex = [|
            [|-1; 0; 1; 2|];
            [|1; -1; 1; 2|];
            [|1; 2; -1; 2|];
            [|1; 2; 3; -1|]
        |]
        Assert.Equal<array<int>>(ex, res) 
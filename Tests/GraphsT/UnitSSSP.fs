namespace UnitSSSP
 
open System 
open Xunit 
open FsCheck 
open FsCheck.Xunit 
open FsCheck.FSharp
open LineralAlgebra

open Graphs.Graphs
open DataAndFuncs.Data
open DataAndFuncs.Funcs


module SSSP = 
    [<Fact>]
    let zeroSSSP () =
        let res = Array.zeroCreate 4
        for i in 0 .. 3 do 
            res.[i] <- SSSP zeroGraph i Array.min (+) -1

        let ex = [|
            [|0; -1; -1; -1|];
            [|-1; 0; -1; -1|];
            [|-1; -1; 0; -1|];
            [|-1; -1; -1; 0|]
        |]
        Assert.Equal<array<array<int>>>(ex, res)

    [<Fact>]
    let fullSSSP () =
        let res = Array.zeroCreate 4
        for i in 0 .. 3 do 
            res.[i] <- SSSP fullGraph i Array.min (+) -1

        let ex = [|
            [|0; 1; 1; 1|];
            [|1; 0; 1; 1|];
            [|1; 1; 0; 1|];
            [|1; 1; 1; 0|]
        |]
        Assert.Equal<array<array<int>>>(ex, res)

    [<Fact>]
    let full2SSSP () =
        let res = Array.zeroCreate 4
        for i in 0 .. 3 do 
            res.[i] <- SSSP fullGraph2 i Array.min (+) -1

        let ex = [|
            [|0; 2; 2; 2|];
            [|2; 0; 2; 2|];
            [|2; 2; 0; 2|];
            [|2; 2; 2; 0|]
        |]
        Assert.Equal<array<array<int>>>(ex, res)

    [<Fact>]
    let dirSSSP () =
        let res = Array.zeroCreate 4
        for i in 0 .. 3 do 
            res.[i] <- SSSP dirGraph i Array.min (+) -1

        let ex = [|
            [|0; 1; 2; 3|];
            [|-1; 0; 1; 2|];
            [|-1; -1; 0; 1|];
            [|-1; -1; -1; 0|]
        |]
        Assert.Equal<array<array<int>>>(ex, res)

    [<Fact>]
    let dir2SSSP () =
        let res = Array.zeroCreate 4
        for i in 0 .. 3 do 
            res.[i] <- SSSP dirGraph2 i Array.min (+) -1

        let ex = [|
            [|0; 2; 4; 6|];
            [|-1; 0; 2; 4|];
            [|-1; -1; 0; 2|];
            [|-1; -1; -1; 0|]
        |]
        Assert.Equal<array<array<int>>>(ex, res)

    [<Fact>]
    let undirSSSP () =
        let res = Array.zeroCreate 4
        for i in 0 .. 3 do 
            res.[i] <- SSSP undirGraph i Array.min (+) -1

        let ex = [|
            [|0; 1; 2; 3|];
            [|1; 0; 1; 2|];
            [|2; 1; 0; 1|];
            [|3; 2; 1; 0|]
        |]
        Assert.Equal<array<array<int>>>(ex, res)

    [<Fact>]
    let undir2SSSP () =
        let res = Array.zeroCreate 4
        for i in 0 .. 3 do 
            res.[i] <- SSSP undirGraph2 i Array.min (+) -1

        let ex = [|
            [|0; 2; 4; 6|];
            [|2; 0; 2; 4|];
            [|4; 2; 0; 2|];
            [|6; 4; 2; 0|]
        |]
        Assert.Equal<array<array<int>>>(ex, res)


module MultiSourceBFS =

    [<Fact>]
    let zeroSSSP () =
        let res = Array.zeroCreate 15

        for i in 0 .. 3 do 
            let combination = combinations 1 [|0; 1; 2; 3|]
            res.[i] <- multiSourceSSSP zeroGraph combination.[i] Array.min (+) -1
        
        for i in 0 .. 5 do 
            let combination = combinations 2 [|0; 1; 2; 3|]
            res.[i + 4] <- multiSourceSSSP zeroGraph combination.[i] Array.min (+) -1

        for i in 0 .. 3 do 
            let combination = combinations 3 [|0; 1; 2; 3|]
            res.[i + 10] <- multiSourceSSSP zeroGraph combination.[i] Array.min (+) -1

        res.[14] <- multiSourceSSSP zeroGraph [|0; 1; 2; 3|] Array.min (+) -1

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
    let zeroSSSP_OrderOfVerticesIsntImportant () =
        let res = multiSourceSSSP zeroGraph [|0; 2|] Array.min (+) -1
        let res1 = multiSourceSSSP zeroGraph [|2; 0|] Array.min (+) -1
        
        let ex = [|0; -1; 0; -1|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let zeroSSSP_RepetitionOfVerticesIsntImportant () =
        let res = multiSourceSSSP zeroGraph [|2|] Array.min (+) -1
        let res1 = multiSourceSSSP zeroGraph [|2; 2|] Array.min (+) -1
        
        let ex = [|-1; -1; 0; -1|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let fullSSSP () =
        let res = Array.zeroCreate 15

        for i in 0 .. 3 do 
            let combination = combinations 1 [|0; 1; 2; 3|]
            res.[i] <- multiSourceSSSP fullGraph combination.[i] Array.min (+) -1
        
        for i in 0 .. 5 do 
            let combination = combinations 2 [|0; 1; 2; 3|]
            res.[i + 4] <- multiSourceSSSP fullGraph combination.[i] Array.min (+) -1

        for i in 0 .. 3 do 
            let combination = combinations 3 [|0; 1; 2; 3|]
            res.[i + 10] <- multiSourceSSSP fullGraph combination.[i] Array.min (+) -1

        res.[14] <- multiSourceSSSP fullGraph [|0; 1; 2; 3|] Array.min (+) -1

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
    let fullSSSP_OrderOfVerticesIsntImportant () =
        let res = multiSourceSSSP fullGraph [|0; 2|] Array.min (+) -1
        let res1 = multiSourceSSSP fullGraph [|2; 0|] Array.min (+) -1
        
        let ex = [|0; 1; 0; 1|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let fullSSSP_RepetitionOfVerticesIsntImportant () =
        let res = multiSourceSSSP fullGraph [|2|] Array.min (+) -1
        let res1 = multiSourceSSSP fullGraph [|2; 2|] Array.min (+) -1
        
        let ex = [|1; 1; 0; 1|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let full2SSSP () =
        let res = Array.zeroCreate 15

        for i in 0 .. 3 do 
            let combination = combinations 1 [|0; 1; 2; 3|]
            res.[i] <- multiSourceSSSP fullGraph2 combination.[i] Array.min (+) -1
        
        for i in 0 .. 5 do 
            let combination = combinations 2 [|0; 1; 2; 3|]
            res.[i + 4] <- multiSourceSSSP fullGraph2 combination.[i] Array.min (+) -1

        for i in 0 .. 3 do 
            let combination = combinations 3 [|0; 1; 2; 3|]
            res.[i + 10] <- multiSourceSSSP fullGraph2 combination.[i] Array.min (+) -1

        res.[14] <- multiSourceSSSP fullGraph2 [|0; 1; 2; 3|] Array.min (+) -1

        let ex = [|
            [|0; 2; 2; 2|];
            [|2; 0; 2; 2|];
            [|2; 2; 0; 2|];
            [|2; 2; 2; 0|];
            [|0; 0; 2; 2|];
            [|0; 2; 0; 2|];
            [|0; 2; 2; 0|];
            [|2; 0; 0; 2|];
            [|2; 0; 2; 0|];
            [|2; 2; 0; 0|];
            [|0; 0; 0; 2|];
            [|0; 0; 2; 0|];
            [|0; 2; 0; 0|];
            [|2; 0; 0; 0|];
            [|0; 0; 0; 0|]
        |]

        for i in 0 .. 14 do 
            Assert.Equal<int>(ex.[i], res.[i])

    [<Fact>]
    let full2SSSP_OrderOfVerticesIsntImportant () =
        let res = multiSourceSSSP fullGraph2 [|0; 2|] Array.min (+) -1
        let res1 = multiSourceSSSP fullGraph2 [|2; 0|] Array.min (+) -1
        
        let ex = [|0; 2; 0; 2|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let full1SSSP_RepetitionOfVerticesIsntImportant () =
        let res = multiSourceSSSP fullGraph2 [|2|] Array.min (+) -1
        let res1 = multiSourceSSSP fullGraph2 [|2; 2|] Array.min (+) -1
        
        let ex = [|2; 2; 0; 2|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let dirSSSP () =
        let res = Array.zeroCreate 15

        for i in 0 .. 3 do 
            let combination = combinations 1 [|0; 1; 2; 3|]
            res.[i] <- multiSourceSSSP dirGraph combination.[i] Array.min (+) -1
        
        for i in 0 .. 5 do 
            let combination = combinations 2 [|0; 1; 2; 3|]
            res.[i + 4] <- multiSourceSSSP dirGraph combination.[i] Array.min (+) -1

        for i in 0 .. 3 do 
            let combination = combinations 3 [|0; 1; 2; 3|]
            res.[i + 10] <- multiSourceSSSP dirGraph combination.[i] Array.min (+) -1

        res.[14] <- multiSourceSSSP dirGraph [|0; 1; 2; 3|] Array.min (+) -1

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
    let dirSSSP_OrderOfVerticesIsntImportant () =
        let res = multiSourceSSSP dirGraph [|0; 2|] Array.min (+) -1
        let res1 = multiSourceSSSP dirGraph [|2; 0|] Array.min (+) -1
        
        let ex = [|0; 1; 0; 1|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let dirSSSP_RepetitionOfVerticesIsntImportant () =
        let res = multiSourceSSSP dirGraph [|2|] Array.min (+) -1
        let res1 = multiSourceSSSP dirGraph [|2; 2|] Array.min (+) -1
        
        let ex = [|-1; -1; 0; 1|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let dir2SSSP () =
        let res = Array.zeroCreate 15

        for i in 0 .. 3 do 
            let combination = combinations 1 [|0; 1; 2; 3|]
            res.[i] <- multiSourceSSSP dirGraph2 combination.[i] Array.min (+) -1
        
        for i in 0 .. 5 do 
            let combination = combinations 2 [|0; 1; 2; 3|]
            res.[i + 4] <- multiSourceSSSP dirGraph2 combination.[i] Array.min (+) -1

        for i in 0 .. 3 do 
            let combination = combinations 3 [|0; 1; 2; 3|]
            res.[i + 10] <- multiSourceSSSP dirGraph2 combination.[i] Array.min (+) -1

        res.[14] <- multiSourceSSSP dirGraph2 [|0; 1; 2; 3|] Array.min (+) -1

        let ex = [|
            [|0; 2; 4; 6|];
            [|-1; 0; 2; 4|];
            [|-1; -1; 0; 2|];
            [|-1; -1; -1; 0|];
            [|0; 0; 2; 4|];
            [|0; 2; 0; 2|];
            [|0; 2; 4; 0|];
            [|-1; 0; 0; 2|];
            [|-1; 0; 2; 0|];
            [|-1; -1; 0; 0|];
            [|0; 0; 0; 2|];
            [|0; 0; 2; 0|];
            [|0; 2; 0; 0|];
            [|-1; 0; 0; 0|];
            [|0; 0; 0; 0|]
        |]

        for i in 0 .. 14 do 
            Assert.Equal<int>(ex.[i], res.[i])

    [<Fact>]
    let dir2SSSP_OrderOfVerticesIsntImportant () =
        let res = multiSourceSSSP dirGraph2 [|0; 2|] Array.min (+) -1
        let res1 = multiSourceSSSP dirGraph2 [|2; 0|] Array.min (+) -1
        
        let ex = [|0; 2; 0; 2|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let dir2SSSP_RepetitionOfVerticesIsntImportant () =
        let res = multiSourceSSSP dirGraph2 [|2|] Array.min (+) -1
        let res1 = multiSourceSSSP dirGraph2 [|2; 2|] Array.min (+) -1
        
        let ex = [|-1; -1; 0; 2|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let undirSSSP () =
        let res = Array.zeroCreate 15

        for i in 0 .. 3 do 
            let combination = combinations 1 [|0; 1; 2; 3|] 
            res.[i] <- multiSourceSSSP undirGraph combination.[i] Array.min (+) -1
        
        for i in 0 .. 5 do 
            let combination = combinations 2 [|0; 1; 2; 3|] 
            res.[i + 4] <- multiSourceSSSP undirGraph combination.[i] Array.min (+) -1

        for i in 0 .. 3 do 
            let combination = combinations 3 [|0; 1; 2; 3|] 
            res.[i + 10] <- multiSourceSSSP undirGraph combination.[i] Array.min (+) -1

        res.[14] <- multiSourceSSSP undirGraph [|0; 1; 2; 3|] Array.min (+) -1

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
    let undirSSSP_OrderOfVerticesIsntImportant () =
        let res = multiSourceSSSP undirGraph [|0; 2|] Array.min (+) -1
        let res1 = multiSourceSSSP undirGraph [|2; 0|] Array.min (+) -1
        
        let ex = [|0; 1; 0; 1|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let undirSSSP_RepetitionOfVerticesIsntImportant () =
        let res = multiSourceSSSP undirGraph [|2|] Array.min (+) -1
        let res1 = multiSourceSSSP undirGraph [|2; 2|] Array.min (+) -1
        
        let ex = [|2; 1; 0; 1|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)


    [<Fact>]
    let undir2SSSP () =
        let res = Array.zeroCreate 15

        for i in 0 .. 3 do 
            let combination = combinations 1 [|0; 1; 2; 3|] 
            res.[i] <- multiSourceSSSP undirGraph2 combination.[i] Array.min (+) -1
        
        for i in 0 .. 5 do 
            let combination = combinations 2 [|0; 1; 2; 3|] 
            res.[i + 4] <- multiSourceSSSP undirGraph2 combination.[i] Array.min (+) -1

        for i in 0 .. 3 do 
            let combination = combinations 3 [|0; 1; 2; 3|] 
            res.[i + 10] <- multiSourceSSSP undirGraph2 combination.[i] Array.min (+) -1

        res.[14] <- multiSourceSSSP undirGraph2 [|0; 1; 2; 3|] Array.min (+) -1

        let ex = [|
            [|0; 2; 4; 6|];
            [|2; 0; 2; 4|];
            [|4; 2; 0; 2|];
            [|6; 4; 2; 0|];
            [|0; 0; 2; 4|];
            [|0; 2; 0; 2|];
            [|0; 2; 2; 0|];
            [|2; 0; 0; 2|];
            [|2; 0; 2; 0|];
            [|4; 2; 0; 0|];
            [|0; 0; 0; 2|];
            [|0; 0; 2; 0|];
            [|0; 2; 0; 0|];
            [|2; 0; 0; 0|];
            [|0; 0; 0; 0|]
        |]

        for i in 0 .. 14 do 
            Assert.Equal<int>(ex.[i], res.[i])

    [<Fact>]
    let undir2SSSP_OrderOfVerticesIsntImportant () =
        let res = multiSourceSSSP undirGraph2 [|0; 2|] Array.min (+) -1
        let res1 = multiSourceSSSP undirGraph2 [|2; 0|] Array.min (+) -1
        
        let ex = [|0; 2; 0; 2|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)

    [<Fact>]
    let undir2SSSP_RepetitionOfVerticesIsntImportant () =
        let res = multiSourceSSSP undirGraph2 [|2|] Array.min (+) -1
        let res1 = multiSourceSSSP undirGraph2 [|2; 2|] Array.min (+) -1
        
        let ex = [|4; 2; 0; 2|]

        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex, res1)
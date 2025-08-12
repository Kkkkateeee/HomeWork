namespace PropertySSSP
 
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
type SSSP() = 
    let power = Random().Next(1, 3)
    let size = pown 2 power

    [<Property>]
    member _.sssp () =
        let graph = GraphGen size "Node"

        for i in 0 .. size - 1 do 
            let res = Graphs.SSSP graph i Array.min (+) -1
            let ex = origSSSP graph [|i|]
            Assert.Equal<int>(ex, res)
    
        
[<Properties(MaxTest = 100)>] 
type MultiSourceSSSP() = 
    let power = Random().Next(1, 3)
    let size = pown 2 power

    [<Property>]
    member _.multiSourceSSSP () =
        let graph = GraphGen size "Node"
        for k in 1 .. graph.n do 
            let combination = combinations k [| for i in 0 .. size - 1 -> i |]
            for c in combination do 
                let res = multiSourceSSSP graph c Array.min (+) -1
                let ex = origSSSP graph c 
                Assert.Equal<int>(ex, res)

    [<Property>]
    member _.orderOfVerticesIsntImportant () =
        let graph = GraphGen size "Node"

        let i = Random().Next(0, size - 1)
        let j = Random().Next(0, size - 1)
        let res = multiSourceSSSP graph [|i; j|] Array.min (+) -1
        let res1 = multiSourceSSSP graph [|j; i|] Array.min (+) -1
        let ex = origSSSP graph [|i; j|]
        let ex1 = origSSSP graph [|j; i|]
        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex1, res1)
        Assert.Equal<int>(res, res1)

    [<Property>]
    member _.repetitionOfVerticesIsntImportant () =
        let graph = GraphGen size "Node"

        let i = Random().Next(0, size - 1)
        let res = multiSourceSSSP graph [|i|] Array.min (+) -1
        let res1 = multiSourceSSSP graph [|i; i|] Array.min (+) -1
        let ex = origSSSP graph [|i|]
        let ex1 = origSSSP graph [|i; i|]
        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex1, res1)
        Assert.Equal<int>(res, res1)
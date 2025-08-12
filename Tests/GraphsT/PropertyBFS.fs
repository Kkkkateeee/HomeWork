namespace PropertyBFS
 
open System 
open Xunit 
open FsCheck 
open FsCheck.Xunit 
open FsCheck.FSharp
open LineralAlgebra

open Graphs
open Graphs.Graphs
open DataAndFuncs.Funcs
open DataAndFuncs.Generator


[<Properties(MaxTest = 100)>] 
type BFS() = 
    let power = Random().Next(1, 3)
    let size = pown 2 power

    [<Property>]
    member _.bfs () =
        let graph = GraphGen size "Node"

        for i in 0 .. size - 1 do 
            let res = Graphs.BFS graph i
            let ex = origBfs graph [|i|]
            Assert.Equal<int>(ex, res)
    
        
[<Properties(MaxTest = 100)>] 
type MultiSourceBFS() = 
    let power = Random().Next(1, 3)
    let size = pown 2 power

    [<Property>]
    member _.multiSourceBFS () =
        let graph = GraphGen size "Node"
        for k in 1 .. graph.n do 
            let combination = combinations k [| for i in 0 .. size - 1 -> i |]
            for c in combination do 
                let res = multiSourceBFS graph c 
                let ex = origBfs graph c 
                Assert.Equal<int>(ex, res)

    [<Property>]
    member _.orderOfVerticesIsntImportant () =
        let graph = GraphGen size "Node"

        let i = Random().Next(0, size - 1)
        let j = Random().Next(0, size - 1)
        let res = multiSourceBFS graph [|i; j|]
        let res1 = multiSourceBFS graph [|j; i|]
        let ex = origBfs graph [|i; j|]
        let ex1 = origBfs graph [|j; i|]
        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex1, res1)
        Assert.Equal<int>(res, res1)

    [<Property>]
    member _.repetitionOfVerticesIsntImportant () =
        let graph = GraphGen size "Node"

        let i = Random().Next(0, size - 1)
        let res = multiSourceBFS graph [|i|]
        let res1 = multiSourceBFS graph [|i; i|]
        let ex = origBfs graph [|i|]
        let ex1 = origBfs graph [|i; i|]
        Assert.Equal<int>(ex, res)
        Assert.Equal<int>(ex1, res1)
        Assert.Equal<int>(res, res1)


[<Properties(MaxTest = 100)>] 
type Reachability() = 
    let power = Random().Next(1, 3) 
    let size = pown 2 power

    [<Property>]
    member _.reachability () =
        let graph = GraphGen size "Node"
        for i in 0 .. size - 1 do 
            for j in 0 .. size - 1 do 
                let res = reachability graph i j 
                let ex = origReach graph i j 
                Assert.Equal<bool>(ex, res)

    [<Property>]
    member _.diagonal () = 
        let graph = GraphGen size "Node"
        for i in 0 .. size - 1 do 
            let res = reachability graph i i
            Assert.True res


[<Properties(MaxTest = 100)>] 
type Parent() = 
    let power = Random().Next(1, 3) 
    let size = pown 2 power

    [<Property>]
    member _.parent () =
        let graph = GraphGen size "Node"
        for i in 0 .. size - 1 do 
            let res = parent graph i
            let ex = origParent graph i 
            Assert.Equal<array<int>>(ex, res)
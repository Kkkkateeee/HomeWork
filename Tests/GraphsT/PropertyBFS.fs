namespace PropertyBFS
 
open System 
open Xunit 
open FsCheck 
open FsCheck.Xunit 
open FsCheck.FSharp
open LineralAlgebra

open Graphs


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

    let toAdjacencyMatrix (graph: Matrix<Edjes<'t>>) =
        let func (value: Edjes<'t>) = 
            match value with 
            | Some _ -> 1
            | None -> 0
        let res = Matrix.map func graph
        res

    // let matrixBFS (graph: Matrix<Edjes<'t>>) (i: int) =

    //     let mutable v = Array.zeroCreate graph.n
    //     v.[i] <- 1
    //     let mutable dist = Array.zeroCreate graph.n
    //     for j in 0 .. graph.n - 1 do 
    //         if j = i then dist.[j] <- 0
    //         else dist.[j] <- -1
    //     let adjM = toAdjacencyMatrix graph
    //     let adj = QTrees.qtreeToArray2D adjM.qtree adjM.n

    //     for j in 1 .. graph.n do 
    //         let res = Array.zeroCreate graph.n
    //         for a in 0 .. graph.n - 1 do 
    //             for b in 0 .. graph.n - 1 do 
    //                 res.[a] <- Graphs.opAdd res.[a] (Graphs.opMult v.[b] adj.[b, a])

    //         v <- Array.map2 Graphs.opAdd v res

    //         for a in 0 .. graph.n - 1 do 
    //             if v.[a] = 1 && dist.[a] = -1 then dist.[a] <- a
    //             elif v.[a] = 1 && dist.[a] = 0 then dist.[a] <- 0
    //             else dist.[a] <- -1
    //     dist

    let matrixBFS (graph: Matrix<Edjes<'t>>) (array: array<int>) =

        let mutable v = Array.zeroCreate graph.n
        for i in 0 .. array.Length - 1 do 
            if Array.exists (fun element -> element = i) array then 
                v.[i] <- 1

        let mutable dist = Array.create graph.n -1
        for i in 0 .. array.Length - 1 do 
            if Array.exists (fun element -> element = i) array then 
                dist.[i] <- 0

        let adjM = toAdjacencyMatrix graph
        let adj = QTrees.qtreeToArray2D adjM.qtree adjM.n

        for j in 1 .. graph.n do 
            let res = Array.zeroCreate graph.n
            for a in 0 .. graph.n - 1 do 
                for b in 0 .. graph.n - 1 do 
                    res.[a] <- Graphs.opAdd res.[a] (Graphs.opMult v.[b] adj.[b, a])

            v <- Array.map2 Graphs.opAdd v res

            for a in 0 .. graph.n - 1 do 
                if v.[a] = 1 && dist.[a] = -1 then dist.[a] <- a
                elif v.[a] = 1 && dist.[a] = 0 then dist.[a] <- 0
                else dist.[a] <- -1
        dist

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

// [<Properties(MaxTest = 100)>] 
// type BFS() = 
    
//     let power = Random().Next(1, 6)
//     let size = pown 2 power

//     [<Property>]
//     member _.bfs () =
//         let graph = GraphGen size "Node"

//         for i in 0 .. size - 1 do 
//             let res = Graphs.BFS graph i
//             let ex = matrixBFS graph [|i|]
//             Assert.Equal<int>(ex, res)
    
        
// [<Properties(MaxTest = 100)>] 
// type MultiSourceBFS() = 
    
//     let power = Random().Next(1, 6)
//     let size = pown 2 power

//     [<Property>]
//     member _.orderOfVerticesIsntImportant () =
//         let graph = GraphGen size "Node"

//         let res = Graphs.multiSourceBFS graph [|0; 1|]
//         let res1 = Graphs.multiSourceBFS graph [|1; 0|]
//         let ex = matrixBFS graph [|0; 1|]
//         let ex1 = matrixBFS graph [|1; 0|]
//         Assert.Equal<int>(ex, res)
//         Assert.Equal<int>(ex1, res1)
//         Assert.Equal<int>(res, res1)


// [<Properties(MaxTest = 100)>] 
// type Reachability() = 
    
//     let power = Random().Next(1, 6) добавить проверку на то что i = j всегда true
//     let size = pown 2 power

//     [<Property>]
//     member _.orderOfVerticesIsntImportant () =
//         let graph = GraphGen size "Node"

//         let res = Graphs.multiSourceBFS graph [|0; 1|]
//         let res1 = Graphs.multiSourceBFS graph [|1; 0|]
//         let ex = matrixBFS graph [|0; 1|]
//         let ex1 = matrixBFS graph [|1; 0|]
//         Assert.Equal<int>(ex, res)
//         Assert.Equal<int>(ex1, res1)
//         Assert.Equal<int>(res, res1)
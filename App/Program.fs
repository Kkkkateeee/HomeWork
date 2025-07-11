
open LineralAlgebra
open Graphs
open Graphs.Graphs


// let rec printQuadtree (tree: QTree<int>) (indent: string) : unit =
//     match tree with
//     | Leaf value -> printfn "%sLeaf: %d" indent value
//     | Node (tl, tr, bl, br) ->
//         printfn "%sNode:" indent
//         printQuadtree tl (indent + "  TL: ")
//         printQuadtree tr (indent + "  TR: ")
//         printQuadtree bl (indent + "  BL: ")
//         printQuadtree br (indent + "  BR: ")

// let matrixBFS1 (graph: Matrix<Edjes<'t>>) (i: int) =

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

// let matrixBFS (graph: Matrix<Edjes<'t>>) (array: array<int>) =

//     let mutable v = Array.zeroCreate graph.n
//     for i in 0 .. array.Length - 1 do 
//         if Array.exists (fun element -> element = i) array then 
//             v.[i] <- 1

//     let mutable dist = Array.create graph.n -1
//     for i in 0 .. array.Length - 1 do 
//         if Array.exists (fun element -> element = i) array then 
//             dist.[i] <- 0

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


// let directed = Node ( 
//     Node ( Leaf None, Leaf (Some [|1|]), Leaf None, Leaf None), 
//     Node ( Leaf None, Leaf None, Leaf (Some [|1|]), Leaf None),
//     Leaf None,
//     Node ( Leaf None, Leaf (Some [|1|]), Leaf None, Leaf None)
// )  
// let dirGraph = { n = 4; qtree = directed }

// let res = matrixBFS1 dirGraph 0

// printf "%A" res

// let a = Graphs.BFS dirGraph 0
// printf "%A" a
let directed = Node ( 
    Node ( Leaf None, Leaf (Some [|1|]), Leaf None, Leaf None), 
    Node ( Leaf None, Leaf None, Leaf (Some [|1|]), Leaf None),
    Leaf None,
    Node ( Leaf None, Leaf (Some [|1|]), Leaf None, Leaf None)
)  
let dirGraph = { n = 4; qtree = directed }

let res = Array.zeroCreate 16

// let ex = [|
//     true; true; true; true;
//     false; true; true; true;
//     false; false; true; true;
//     false; false; false; true
// |]

for i in 0 .. 3 do 
   // printf "%A" (BFS dirGraph i )
    for j in 0 .. 3 do 
        //printf "%d %d\n" i j
        res.[4 * i + j] <- reachability dirGraph i j


printf "%A" res
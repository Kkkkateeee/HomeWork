namespace Graphs

open LineralAlgebra

(*
Graph<Matrix<QTree<Array<'t>>>>


*)

type Edjes<'t> =
    | Array of 't
    | None


type Graph<'t> =
    | None
    | Some of 't


module Graphs =
    let intQTree1 = Node ( 
        Node ( Leaf [|1; 2|], Leaf [|2|], Leaf [|3; 4|], Leaf [|2;8|]), 
        Node ( Leaf [| 3; 6|], Leaf [|-6|], Leaf [|1; 8|], Leaf [|0|]), 
        Leaf [|9|],  
        Node ( Leaf [|1; 2|], Leaf [|0; 9|], Leaf [|0; 9|], Leaf [|9; 2|])  
        ) 

    let m = { n = 4; qtree = intQTree1 }
    let g = Some m
    
    let transitiveClosure (graph: Graph<'t>) : Graph<'t> =
        graph


(*
      A B C D E F G H
   A  0 0 0 0 1 2 0 0
   B  0 0 0 0 3 0 0 0
   C  9 9 9 0 0 0 1 0
   D  9 9 9 0 0 0 1 0
   E  1 1 1 1 1 1 1 1
   F  0 0 0 0 0 0 0 0
   G  0 0 0 0 0 0 0 0 
*)

    // let shortestWay (graph: Graph<'t>) ()=
    //     match graph with 
    //     | None -> failwith "Your graph does not exist" 
    //     | Some value -> 

    // let a = 
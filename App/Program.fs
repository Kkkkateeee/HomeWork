open LineralAlgebra


module Graphs =

//     let transitiveClosure (graph: Matrix<Edjes<'t>>) (opAddForArrayElements: 't -> 't -> 't) = 
//         let opAddForEdjes edje1 edje2 = 
//             match edje1, edje2 with 
//             | None, None -> None
//             | None, Some array -> Some array
//             | Some array, None -> Some array
//             | Some array1, Some array2 -> 
//                 let array = 
//                     [|
//                         for i in 0 .. Array.length array1 - 1 do  
//                             for j in 0 .. Array.length array2 - 1 do
//                                 yield opAddForArrayElements array1[i] array2[j]
//                     |]
//                 Some array

//         let opMultForEdjes edje1 edje2 =
//             match edje1, edje2 with 
//             | None, None -> None
//             | None, Some array -> Some array
//             | Some array, None -> Some array
//             | Some array1, Some array2 -> Some (Array.append array1 array2)

//         let res = Matrix.multiply graph graph opAddForEdjes opMultForEdjes
//         res




//     let node = Node ( 
//         Node (Leaf None, Leaf None, Leaf None, Leaf None),
//         Node (Leaf None, Leaf None, Leaf None, Leaf (Some [|1; 2; 3|])), 
//         Node (Leaf None, Leaf None, Leaf None, Leaf None),
//         Node (Leaf None, Leaf None, Leaf (Some [|2|]), Leaf None)
//         )     

// (*
// 0 0 2 3
// 0 0 4 1
// 0 0 0 0 
// 0 0 1 0

// *)

//     let Gnode = { n = 4; qtree = node }

//     let v = transitiveClosure Gnode (+)
//     let a = QTrees.qtreeToArray2D v.qtree v.n



//     // let trCl (matrix: int array array) =  
    //     let closure = Array2D.zeroCreate 4 4
    //     for x in 0 .. 3 do
    //         for y in 0 .. 3 do
    //             for z in 0 .. 3 - 1 do
    //                 if matrix.[y].[x] <> 0 && matrix.[x].[z] <> 0 then
    //                     // closure[y, z] <- matrix[y].[x] + matrix.[x].[z]
    //                     closure[y, z] <- 1

    //     closure
                        
        
        


    // let matrix = 
    //     [|
    //         [|0; 0; 0; 0|];
    //         [|0; 0; 0; 1|];
    //         [|0; 0; 0; 0|];
    //         [|0; 0; 1; 0|]
    //     |]
(*
        A B <C> <D> 
 i   A  0 0  0   0  j
     B->0 0 1+1 (1)
     C->0 0  0   0 
     D->0 0  1   0  

            [|0; 1; 0; 0|];
            [|2; 3; 0; 0|];
            [|10; 10; 0; 12|];
            [|10; 10; 14; 0|]
*)

    // let newM = trCl matrix
    // for i in 0 .. 3 do
    //     for j in 0 .. 3 do
    //         match a[i, j] with 
    //         | None -> printf "%d " 0
    //         | Some array -> printf "%A " array
    //     printf "\n"


    let leafArr = Leaf (Some [|1; 2; 3|])
    let GleafArr1 = { n = 1; qtree = leafArr }
    let GleafArr4 = { n = 4; qtree = leafArr }

    if GleafArr1 = GleafArr4 then printf "a\n"
    else printf "b\n"
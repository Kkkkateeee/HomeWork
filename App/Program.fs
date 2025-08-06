open System.Threading.Tasks

let height = 10
let width = 20

// Parallel.For(0, height * width, fun index ->
//     let i = index / width  
//     let j = index % width  

//     printfn "i: %d, j: %d, Thread: %d" i j System.Threading.Thread.CurrentThread.ManagedThreadId
// ) |> ignore

(*
0 .. 10
..
20
*)

// for k in 0 .. 10 * 20 - 1 do
//     let i = k / 10
//     let j = k % 10
//     printfn "%d %d %d" k  i j 
let a = 5 / 2

printfn "%d" a
(*
добавить private для opAdd opMult toAdj
проверить, везде ли индексация начинается с 0
сделать более понятные имена (например для v)
проверить, что в главных функциях есть ограничение для входных параметров нв уровне компилятора
убрать лишние комментарии
проверить все функции и если что дописать проверки
?? дописать функцию которая превращает ориентир граф в неориент
сделать отдельный общий файл с дата (сделать обшме графы) 
добавить param, который показывает изменения
убрать желтые предупреждения
дописать матрксбфс и проперти тесты к бфс
*)
// #nowarn 0025

namespace Graphs

open LineralAlgebra


type Edjes<'t> =
    | Some of array<'t>
    | None
    member this.min (func: array<'t> -> 't) =
        match this with 
        | None -> failwith "Edges data type is None"
        | Some array -> func array
            

module Graphs =



    let rec findValue (qtree: QTree<'t>) (size: int) i j : 't = 
        match qtree with 
        | Leaf value -> value
        | Node(nw, ne, sw, se) ->
            if i <= size / 2 && j <= size / 2 then 
                findValue nw (size / 2) i j
            elif i <= size / 2 && j > size / 2 then 
                findValue ne (size / 2) i (j / 2)
            elif i > size / 2 && j <= size / 2 then 
                findValue sw (size / 2) (i / 2) j
            else 
                findValue se (size / 2) (i / 2) (j / 2)

    let toAdjacencyMatrix (graph: Matrix<Edjes<'t>>) =
        let func (value: Edjes<'t>) = 
            match value with 
            | Some _ -> 1
            | None -> 0
        let res = Matrix.map func graph
        res

    let opMult x y =
        if x = 1 && y = 1 then 1 
        else 0 
            
    let opAdd x y =
        if x = 0 && y = 0 then 0 
        else 1 


    let vOrDistBuilder (n: int) (vertexArray: array<int>) (param: string) : Matrix<int> =
        let rec builder (rowStart: int) (colStart: int) (size: int) (param: string) : QTree<int> =
            if size = 1 then 
                if param = "v" then 
                    if Array.exists (fun element -> element = colStart) vertexArray then 
                        Leaf 1
                    else
                        Leaf 0

                else 
                    if Array.exists (fun element -> element = colStart) vertexArray then 
                        Leaf 0
                    else
                            Leaf -1
            else
                let halfSize = size / 2

                let NW = builder rowStart colStart halfSize param
                let NE = builder rowStart (colStart + halfSize) halfSize param
                let SW = builder (rowStart + halfSize) colStart halfSize param
                let SE = builder (rowStart + halfSize) (colStart + halfSize) halfSize param

                Node(NW, NE, SW, SE)

        { n = n; qtree = builder 0 0 n param }


    let distReset (v: Matrix<int>) (dist: Matrix<int>) (j: int) =
        let func j =
            let f1 v d = 
                match v, d with 
                | 1, -1 -> j
                | 1, 0 -> 0
                | 1, value -> value 
                | 0, -1 -> -1
            f1 

        let res = Matrix.map2 (func j) v dist
        res

    let matrixToArray (matr: Matrix<int>) =
        let n = matr.n 

        let rec getElement (qtree: QTree<int>) (size: int) (row: int) (col: int) : int =
            match qtree with
            | Leaf value -> value
            | Node(nw, ne, sw, se) ->
                let halfSize = size / 2
                if row < halfSize then 
                    if col < halfSize then 
                        getElement nw halfSize row col
                    else 
                        getElement ne halfSize row (col - halfSize)
                else 
                    if col < halfSize then 
                        getElement sw halfSize (row - halfSize) col
                    else 
                        getElement se halfSize (row - halfSize) (col - halfSize)

        let res = Array.zeroCreate n
        for col in 0 .. n - 1 do
            res.[col] <- getElement matr.qtree n 0 col 
        res


    /// <summary>Finds the shortest path between vertices i and j of the graph</summary>
    /// <param name="graph">a graph in which the shortest path must be found</param>
    /// <param name="i">row coordinate of the adjacency matrix of the graph</param>
    /// <param name="j">column coordinate of the adjacency matrix of a graph</param>
    /// <param name="func">finds mim 't in 't array</param>
    /// <returns>Shortest path length</returns>
    let shortestWay (graph: Matrix<Edjes<'t>>) (i: int) (j: int) (func: array<'t> -> 't) : 't =
        if i <= 0 then failwithf "\n\nThe row coordinate i must be > 0\nYour i: %d\n" i
        elif j <= 0 then failwithf "\n\nThe column coordinate j must be > 0\nYour j: %d\n" j
        elif i > graph.n then failwithf "\n\nThis row coordinate does not exist\nMaximum column coordinate: %d\nYour coordinate: %d\n" graph.n i
        elif j > graph.n then failwithf "\n\nThis column coordinate does not exist\nMaximum row coordinate: %d\nYour coordinate: %d\n" graph.n j

        let handleLeaf edjes =
            match edjes with 
            | None -> 
                printfn "There is no path from %d to %d" i j
                Unchecked.defaultof<'t>
            | Some _ -> 
                edjes.min func

        let rec findValue (qtree: QTree<Edjes<'t>>) (size: int) i j : 't = 
            match qtree with 
            | Leaf edjes -> handleLeaf edjes
            | Node(nw, ne, sw, se) ->
                if i <= size / 2 && j <= size / 2 then 
                    findValue nw (size / 2) i j
                elif i <= size / 2 && j > size / 2 then 
                    findValue ne (size / 2) i (j / 2)
                elif i > size / 2 && j <= size / 2 then 
                    findValue sw (size / 2) (i / 2) j
                else 
                    findValue se (size / 2) (i / 2) (j / 2)

        let value = findValue graph.qtree graph.n i j
        value
        // ЗДЕСЬ I J НАЧИНАЮТСЯ С 1


    /// <summary>Creates a new graph that is the transitive closure of the underected graph given as argument</summary>
    /// <param name="graph">the graph whose transitive closure is to be constructed</param>
    /// <returns>Transitive closure graph</returns>
    let transitiveClosure (graph: Matrix<Edjes<'t>>) : Matrix<int> = 
        let adjM = toAdjacencyMatrix graph
        let mutable result = adjM

        for i in 2 .. graph.n do
            let power = Matrix.multiply result adjM opAdd opMult
            result <- Matrix.map2 opAdd result power

        result

(*
Для графов из предыдущей задачи реализовать алгоритмы 
обхода в ширину 

от одного - массив кратчайних путей от 1 источнимка
нескольких источников - массив путей от нескольких источников
достижимость, 
parent

Такие же вариации SSSP
*)

    /// <summary>Performs a BFS of a graph and produces an array of shortest distances from the input vertex i to all others</summary>
    /// <param name="graph"></param>
    /// <param name="vertexArray"></param>
    /// <typeparam name="'t"></typeparam>
    /// <typeparam name="'a"></typeparam>
    /// <returns></returns>
    let multiSourceBFS (graph: Matrix<Edjes<'t>>) (vertexArray: array<int>) : array<int> =
        if vertexArray.Length > graph.n then failwithf "\n\n The length of the vertex array cannot be greater than the number of vertices in the graph\n\n "//дописать
        for i in 0 .. vertexArray.Length - 1 do 
            if vertexArray.[i] < 0 || vertexArray.[i] > graph.n then failwithf "\n\n i must be in the range from 0 to graph.n - 1\n\n your i: %d\n" i

        let mutable v = vOrDistBuilder graph.n vertexArray "v" 
        let mutable dist = vOrDistBuilder graph.n vertexArray "dist" 
        let adjM = toAdjacencyMatrix graph 

        for j in 1 .. graph.n do 
            let res = Matrix.multiply v adjM opAdd opMult
            v <- Matrix.map2 opAdd v res
            dist <- distReset v dist j

        matrixToArray dist

    /// <summary>Performs a BFS of a graph and produces an array of shortest distances from the input vertex i to all others</summary>
    /// <param name="graph">graph relative to which bfs needs to be done</param>
    /// <param name="i">the vertex relative to which the shortest paths must be found. Vertices are numbered from 0 to n - 1</param>
    /// <returns>array with shortest distances to other vertices. Distance from the i to itself: 0. If the path does not exist: -1</returns>
    let BFS (graph: Matrix<Edjes<'t>>) (i: int) : array<int> =  
        if i < 0 || i > graph.n - 1 then failwithf "\n\n i must be in the range from 0 to graph.n - 1\n\n your i: %d\n" i

        let res = multiSourceBFS graph [|i|]
        res


    /// <summary></summary>
    /// <param name="graph"></param>
    /// <param name="i"></param>
    /// <param name="j"></param>
    /// <typeparam name="'t"></typeparam>
    /// <returns></returns>
    let reachability (graph: Matrix<Edjes<'t>>) (i: int) (j: int) : bool =
        if i < 0 || i > graph.n - 1 then failwithf "\n\n i must be in the range from 0 to graph.n - 1\n\n your i: %d\n" i
        elif j < 0 || j > graph.n - 1 then failwithf "\n\n j must be in the range from 0 to graph.n - 1\n\n your j: %d\n" j
        else
            let res = BFS graph i
            if res.[j] = -1 then false
            else true


    /// <summary></summary>
    /// <param name="graph"></param>
    /// <param name="i"></param>
    /// <typeparam name="'t"></typeparam>
    /// <typeparam name="'a"></typeparam>
    /// <returns></returns>
    let parent (graph: Matrix<Edjes<'t>>) (i: int) =
        if i < 0 || i > graph.n - 1 then failwithf "\n\n i must be in the range from 0 to graph.n - 1\n\n your i: %d\n" i

        let parents = Array.zeroCreate graph.n

        let mutable v = vOrDistBuilder graph.n [|i|] "v" 
        let mutable dist = vOrDistBuilder graph.n [|i|] "dist" 
        let adjM = toAdjacencyMatrix graph 

        for j in 1 .. graph.n do 
            let res = Matrix.multiply v adjM opAdd opMult
            v <- Matrix.map2 opAdd v res
            dist <- distReset v dist j

        matrixToArray dist
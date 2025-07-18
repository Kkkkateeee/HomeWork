namespace Graphs

open LineralAlgebra
open System


type Edjes<'t> =
    | Some of array<'t>
    | None
    member this.Min (func: array<'t> -> 't) =
        match this with 
        | None -> failwith "Edges data type is None"
        | Some array -> func array
            

module Graphs =
    let rec private findValue (qtree: QTree<'t>) (size: int) i j  =
        match qtree with
        | Leaf value -> value
        | Node(nw, ne, sw, se) ->
            if i < size / 2 && j < size / 2 then
                findValue nw (size / 2) i j
            elif i < size / 2 && j >= size / 2 then
                findValue ne (size / 2) i (j - size / 2)
            elif i >= size / 2 && j < size / 2 then
                findValue sw (size / 2) (i - size / 2) j
            else
                findValue se (size / 2) (i - size / 2) (j - size / 2)

    let private toAdjacencyMatrix (graph: Matrix<Edjes<'t>>) =
        let func (value: Edjes<'t>) = 
            match value with 
            | Some _ -> 1
            | None -> 0
        let res = Matrix.map func graph
        res

    let private toWeightedAdjacencyMatrix (garph: Matrix<Edjes<'t>>) (minFunc: array<'t> -> 't) (inf: 't) = 
        let rec buildQTree (qtree: QTree<Edjes<'t>>) (row: int, col: int, size: int) =
            match qtree with 
            | Node(nw, ne, sw, se) ->
                    let halfSize = size / 2
                    let topLeft = buildQTree nw (row, col, halfSize)
                    let topRight = buildQTree ne (row, col + halfSize, halfSize)
                    let bottomLeft = buildQTree sw (row + halfSize, col, halfSize)
                    let bottomRight = buildQTree se (row + halfSize, col + halfSize, halfSize)
                    Node (topLeft, topRight, bottomLeft, bottomRight)

            | Leaf edjes when size > 1 ->
                    let halfSize = size / 2
                    let topLeft = buildQTree (Leaf edjes) (row, col, halfSize)
                    let topRight = buildQTree (Leaf edjes) (row, col + halfSize, halfSize)
                    let bottomLeft = buildQTree (Leaf edjes) (row + halfSize, col, halfSize)
                    let bottomRight = buildQTree (Leaf edjes) (row + halfSize, col + halfSize, halfSize)
                    Node (topLeft, topRight, bottomLeft, bottomRight)

            | Leaf edjes when size = 1 ->
                if row = col then
                    Leaf Unchecked.defaultof<'t>
                else
                    match edjes with 
                        | Some arr -> Leaf (minFunc arr)
                        | None -> Leaf inf

            | Leaf _ -> failwith "Not Implemented"

        { n = garph.n; qtree = buildQTree garph.qtree (0, 0, garph.n) }

    let private opMult x y =
        if x = 1 && y = 1 then 1 
        else 0 
            
    let private opAdd x y =
        if x = 0 && y = 0 then 0 
        else 1 

    let private vOrDistBuilder (n: int) (vertexArray: array<int>) (param: string) : Matrix<int> =
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

    let private distReset (v: Matrix<int>) (dist: Matrix<int>) (j: int) =
        let func j =
            let f1 v d = 
                match v, d with 
                | 1, -1 -> j
                | 1, 0 -> 0
                | 1, value -> value 
                | 0, -1 -> -1
                | _, _ -> failwithf "Not Implemented"
            f1 

        let res = Matrix.map2 (func j) v dist
        res

    let private getRowOfMatrix (matr: Matrix<'t>) (i: int) =
        let n = matr.n 

        let rec getElement (qtree: QTree<'t>) (size: int) (row: int) (col: int) =
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
            res.[col] <- getElement matr.qtree n i col 
        res


    /// <summary>Finds the shortest path between vertices i and j of the graph.</summary>
    /// <param name="graph">A graph in which the shortest path must be found.</param>
    /// <param name="i">Row coordinate of the adjacency matrix of the graph.</param>
    /// <param name="j">Column coordinate of the adjacency matrix of a graph.</param>
    /// <param name="func">Finds mim 't in 't array.</param>
    /// <returns>Shortest path length.</returns>
    let shortestWay (graph: Matrix<Edjes<'t>>) (i: int) (j: int) (func: array<'t> -> 't) =
        if i < 0 then failwithf "\n\nThe row coordinate i must be > 0\nYour i: %d\n" i
        elif j < 0 then failwithf "\n\nThe column coordinate j must be > 0\nYour j: %d\n" j
        elif i >= graph.n then failwithf "\n\nThis row coordinate does not exist\nMaximum column coordinate: %d\nYour coordinate: %d\n" graph.n i
        elif j >= graph.n then failwithf "\n\nThis column coordinate does not exist\nMaximum row coordinate: %d\nYour coordinate: %d\n" graph.n j

        let handleLeaf edjes =
            match edjes with 
            | None -> 
                printfn "There is no path from %d to %d" i j
                Unchecked.defaultof<'t>
            | Some _ -> 
                edjes.Min func

        let rec findValue (qtree: QTree<Edjes<'t>>) (size: int) i j : 't = 
            match qtree with
            | Leaf value -> handleLeaf value
            | Node(nw, ne, sw, se) ->
                if i < size / 2 && j < size / 2 then
                    findValue nw (size / 2) i j
                elif i < size / 2 && j >= size / 2 then
                    findValue ne (size / 2) i (j - size / 2)
                elif i >= size / 2 && j < size / 2 then
                    findValue sw (size / 2) (i - size / 2) j
                else
                    findValue se (size / 2) (i - size / 2) (j - size / 2)

        let value = findValue graph.qtree graph.n i j
        value


    /// <summary>Creates a new graph that is the transitive closure of the underected graph given as argument.</summary>
    /// <param name="graph">The graph whose transitive closure is to be constructed.</param>
    /// <returns>Transitive closure graph.</returns>
    let transitiveClosure (graph: Matrix<Edjes<'t>>) = 
        let adjM = toAdjacencyMatrix graph
        let mutable result = adjM

        for i in 2 .. graph.n do
            let power = Matrix.multiply result adjM opAdd opMult 0
            result <- Matrix.map2 opAdd result power

        result


    /// <summary>Performs a BFS of a graph and produces an array of shortest distances from the input vertex i to all others</summary>
    /// <param name="graph">The input graph.</param>
    /// <param name="vertexArray">Array of vertices from which bfs will start.</param>
    /// <returns>An array of distances from all vertices to the rest.</returns>
    let multiSourceBFS (graph: Matrix<Edjes<'t>>) (vertexArray: array<int>) =
        if vertexArray.Length > graph.n then failwithf "\n\n The length of the vertex array cannot be greater than the number of vertices in the graph\n\n "
        for i in 0 .. vertexArray.Length - 1 do 
            if vertexArray.[i] < 0 || vertexArray.[i] > graph.n then failwithf "\n\n i must be in the range from 0 to graph.n - 1\n\n your i: %d\n" i

        let mutable v = vOrDistBuilder graph.n vertexArray "v" 
        let mutable dist = vOrDistBuilder graph.n vertexArray "dist" 
        let adjM = toAdjacencyMatrix graph 

        for j in 1 .. graph.n do 
            let res = Matrix.multiply v adjM opAdd opMult 0
            v <- Matrix.map2 opAdd v res 
            dist <- distReset v dist j

        getRowOfMatrix dist 0


    /// <summary>Performs a BFS of a graph and produces an array of shortest distances from the input vertex i to all others.</summary>
    /// <param name="graph">Graph relative to which bfs needs to be done.</param>
    /// <param name="i">The vertex relative to which the shortest paths must be found. Vertices are numbered from 0 to n - 1.</param>
    /// <returns>Array with shortest distances to other vertices. Distance from the i to itself: 0. If the path does not exist: -1.</returns>
    let BFS (graph: Matrix<Edjes<'t>>) (i: int) =  
        if i < 0 || i > graph.n - 1 then failwithf "\n\n i must be in the range from 0 to graph.n - 1\n\n your i: %d\n" i

        let res = multiSourceBFS graph [|i|]
        res


    /// <summary>Checks if there is an edge between vertices i and j.</summary>
    /// <param name="graph">The input graph.</param>
    /// <param name="i">The first input vertex.</param>
    /// <param name="j">The second input vertex.</param>
    /// <returns>True if there is a edje, false if there is not.</returns>
    let reachability (graph: Matrix<Edjes<'t>>) (i: int) (j: int) =
        if i < 0 || i > graph.n - 1 then failwithf "\n\n i must be in the range from 0 to graph.n - 1\n\n your i: %d\n" i
        elif j < 0 || j > graph.n - 1 then failwithf "\n\n j must be in the range from 0 to graph.n - 1\n\n your j: %d\n" j
        else
            let res = BFS graph i
            if res.[j] = -1 then false
            else true


    /// <summary>Traverses the graph using bfs from vertex i, and for each vertex calculates from which vertex the bfs came.</summary>
    /// <param name="graph">The input graph.</param>
    /// <param name="i">The vertex from which the bfs starts.</param>
    /// <returns>Array of parent nodes.</returns>
    let parent (graph: Matrix<Edjes<'t>>) (i: int) =
        let dist = multiSourceBFS graph [|i|]
        let parents = Array.create graph.n -1

        for k in 0 .. graph.n - 1 do
            if k <> i then 
                if dist.[k] >= 0 then 
                    for j in 0 .. graph.n - 1 do 

                        if findValue (toAdjacencyMatrix graph).qtree graph.n j k = 1 && dist.[j] = dist.[k] - 1 then 
                            parents.[k] <- j
        parents


    /// <summary>Finds the shortest distances from a given vertices to the rest.</summary>
    /// <param name="graph">The input graph.</param>
    /// <param name="vertexArray">Array of vertices from which to search for shortest paths.</param>
    /// <param name="minFunc">A function that finds the minimum 't in an array of 't.</param>
    /// <param name="opAdd">Operation of addition of 't.</param>
    /// <param name="inf">Element denoting infinity.</param>
    /// <returns>Array of arrays of shortest distances from each vertex in the vertexArray.</returns>
    let multiSourceSSSP (graph: Matrix<Edjes<'t>>) (vertexArray: array<int>) (minFunc: array<'t> -> 't) (opAdd: 't -> 't -> 't) (inf: 't) = 
        if vertexArray.Length > graph.n then failwithf "\n\n The length of the vertex array cannot be greater than the number of vertices in the graph\n\n "
        for i in 0 .. vertexArray.Length - 1 do 
            if vertexArray.[i] < 0 || vertexArray.[i] > graph.n then failwithf "\n\n i must be in the range from 0 to graph.n - 1\n\n your i: %d\n" i

        let opAdd1 x y =
            if x = inf && y = inf then inf
            elif x = inf && y <> inf then y
            elif y = inf && x <> inf then x
            else minFunc [|x; y|]

        let opMult1 x y =
            if x = inf || y = inf then inf
            else opAdd x y

        let adjM = toWeightedAdjacencyMatrix graph minFunc inf  
        let mutable power = adjM
        
        for i in 0 .. graph.n - 1 do 
            power <- Matrix.multiply power adjM opAdd1 opMult1 inf

        let res = Array.zeroCreate vertexArray.Length
        for i in 0 .. vertexArray.Length - 1 do 
            res.[i] <- getRowOfMatrix power vertexArray.[i]
            
        let mutable res2 = Array.create graph.n inf 
        for i in 0 .. res.Length - 1 do 
            res2 <- Array.map2 opAdd1 res2 res.[i]
        res2


    /// <summary>Finds the shortest distances from a given vertex to the rest.</summary>
    /// <param name="graph">The input graph.</param>
    /// <param name="i">The peak from which to look for the shortest paths.</param>
    /// <param name="minFunc">A function that finds the minimum 't in an array of 't.</param>
    /// <param name="opAdd">Operation of addition of 't.</param>
    /// <param name="inf">Element denoting infinity.</param>
    /// <returns>Array of shortest distances.</returns>
    let SSSP (graph: Matrix<Edjes<'t>>) (i: int) (minFunc: array<'t> -> 't) (opAdd: 't -> 't -> 't) (inf: 't) = 
        if i < 0 || i > graph.n - 1 then failwithf "\n\n i must be in the range from 0 to graph.n - 1\n\n your i: %d\n" i
        let res = multiSourceSSSP graph [|i|] minFunc opAdd inf
        res
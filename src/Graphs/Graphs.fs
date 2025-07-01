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


    /// <summary>Creates a new graph that is the transitive closure of the graph given as argument</summary>
    /// <param name="graph">the graph whose transitive closure is to be constructed</param>
    /// <returns>Transitive closure graph</returns>
    let transitiveClosure (graph: Matrix<Edjes<'t>>) = 
        let toAdjacencyMatrix (graph: Matrix<Edjes<'t>>) =
            let rec toAdjacencyQTree (qtree: QTree<Edjes<'t>>) =
                match qtree with 
                | Leaf edjes -> 
                    match edjes with 
                    | Some _ -> Leaf 1
                    | None -> Leaf 0
                | Node(nw, ne, sw, se) -> 
                    let NW = toAdjacencyQTree nw      
                    let NE = toAdjacencyQTree ne
                    let SW = toAdjacencyQTree sw
                    let SE = toAdjacencyQTree se
                    Node(NW, NE, SW, SE)  

            let res = toAdjacencyQTree graph.qtree
            { n = graph.n; qtree = res }

        let valueReset (matrix: Matrix<int>) =
            let rec valueResetQTree (qtree: QTree<int>) =
                match matrix.qtree with 
                | Leaf value when value = 1 -> Leaf 1
                | Leaf value when value > 1 -> Leaf 1
                | Node (nw, ne, sw, se) ->
                    let NW = valueResetQTree nw      
                    let NE = valueResetQTree ne
                    let SW = valueResetQTree sw
                    let SE = valueResetQTree se
                    Node(NW, NE, SW, SE)
                | Leaf(_) -> failwith "Not Implemented"  
                
            let res = valueResetQTree matrix.qtree
            { n = matrix.n; qtree = res }


        let mutable adjM = toAdjacencyMatrix graph
       
        let mutable param = true 
        while param do
            if valueReset (adjM + Matrix.multiply adjM adjM (+) ( * )) = adjM then param <- false
            else 
                adjM <- valueReset (adjM + Matrix.multiply adjM adjM (+) ( * )) 
            

        adjM

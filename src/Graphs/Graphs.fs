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

    let private opMult x y =
        if x = 1 && y = 1 then 1 
        else 0 
            
    let private opAdd x y =
        if x = 0 && y = 0 then 0 
        else 1 


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
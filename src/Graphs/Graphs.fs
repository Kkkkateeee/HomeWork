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


    let builder (qtreeOriginal: QTree<Edjes<'t>>) (set: Set<int * int * array<'t>>) =
        qtreeOriginal

    let rec finder (qtree: QTree<Edjes<'t>>) (size: int) (i: int) (j: int) (set: Set<int * int * array<'t>>) =
        match qtree with 
        | Leaf edjes ->
            match edjes with 
            | None -> set
            | Some array -> set.Add (i, j, array)

        | Node(nw, ne, sw, se) ->
            if i <= size / 2 && j <= size / 2 then 
                finder nw (size / 2) i j set
            elif i <= size / 2 && j > size / 2 then 
                finder ne (size / 2) i (j - size / 2) set 
            elif i > size / 2 && j <= size / 2 then 
                finder sw (size / 2) (i - size / 2) j set 
            else 
                finder se (size / 2) (i - size / 2) (j - size / 2) set

    let rec a (qtree: QTree<Edjes<'t>>) size i j =  // в i j подается size
        match qtree with 
        | Node(nw, ne, sw, se) ->
            a nw (size / 2) (i / 2) (j / 2)
            a ne (size / 2) (i / 2) j
            a sw (size / 2) i (j / 2)
            a se (size / 2) i j

        | Leaf edjes when size > 1 ->
            a (Leaf edjes) (size / 2) (i / 2) (j / 2)
            a (Leaf edjes) (size / 2) (i / 2) j
            a (Leaf edjes) (size / 2) i (j / 2)
            a (Leaf edjes) (size / 2) i j
        
        | Leaf edges when size = 1 -> 
            match edges with 
            | None -> ignore
            | Some array -> finder qtree size i j Set.empty
        
        

    (*
    nw ne  11 12
    sw se  21 22
    (set: Set<int * int * array<'t>>)
    *)

    /// <summary>Creates a new graph that is the transitive closure of the graph given as argument</summary>
    /// <param name="graph">the graph whose transitive closure is to be constructed</param>
    /// <returns>Transitive closure graph</returns>
    let transitiveClosure (graph: Matrix<Edjes<'t>>) =
        let set = a graph.qtree graph.n graph.n graph.n
        let qtree = builder graph.qtree |> toC
        { n = graph.n  }
        

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
        
    (*
    nw ne
    sw se
    (set: Set<int * int * array<'t>>)
    *)
(*      i
        A B C D E  <F>  <G> H
     A  0 0 0 0 1   2    0  0
 j   B  0 0 0 0 3   0    0  0
     C->9 9 9 0 0  1+1  (1) 0
     D  9 9 9 0 0   0    1  0
     E  1 1 1 1 1   1    1  1
     F  0 0 0 0 0   0    0  0
     G->0 0 0 0 0  (1)   0  0
     H->0 0 0 0 0   1    0  0 
*) 
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

    let rec collectElements tree =
        match tree with
        | Leaf element -> Set.singleton element
        | Node (nw, ne, sw, se) ->
            Set.unionMany 
                [
                            collectElements nw; 
                            collectElements ne; 
                            collectElements sw; 
                            collectElements se
                        ]

    let rec buildTransitiveClosure tree allElements =
        match tree with
        | Leaf element ->
            Leaf element

        | Node (nw, ne, sw, se) ->

            let newNw = buildTransitiveClosure nw allElements
            let newNe = buildTransitiveClosure ne allElements
            let newSw = buildTransitiveClosure sw allElements
            let newSe = buildTransitiveClosure se allElements

            Node (newNw, newNe, newSw, newSe)

    let transitiveClosure (matrix: Matrix<'t>) : Matrix<'t> =
        let allElements = collectElements matrix.qtree
        let newQTree = buildTransitiveClosure matrix.qtree allElements
        { n = matrix.n; qtree = newQTree }


    // /// <summary>Creates a new graph that is the transitive closure of the graph given as argument</summary>
    // /// <param name="graph">the graph whose transitive closure is to be constructed</param>
    // /// <returns>Transitive closure graph</returns>
    // let transitiveClosure (qtree: QTree<int>) : Map<int, Set<int>> =
    //     let rec findReachable (qtree: QTree<int>) (reachable: Set<int>) =
    //         match qtree with
    //         | Leaf value ->
    //             Set.add value reachable
    //         | Node(nw, ne, se, sw) ->
    //             let reachableFromNW = findReachable nw reachable
    //             let reachableFromNE = findReachable ne reachableFromNW
    //             let reachableFromSE = findReachable se reachableFromNE
    //             findReachable sw reachableFromSE

    //     // let rec findAllReachableFrom node =
    //     //     match node with
    //     //     | Leaf value -> 
    //     //         value, Set.singleton value 
    //     //     | Node(nw, ne, se, sw) ->
    //     //         let nwValue, nwReachable = findAllReachableFrom nw
    //     //         let neValue, neReachable = findAllReachableFrom ne
    //     //         let seValue, seReachable = findAllReachableFrom se
    //     //         let swValue, swReachable = findAllReachableFrom sw

    //     //         let allReachable = Set.unionMany [nwReachable; neReachable; seReachable; swReachable]
    //     //         nwValue, allReachable

    //     let rec buildClosure qtree closure =
    //         match qtree with
    //         | Leaf value ->
    //             Map.add value (Set.singleton value) closure
    //         | Node(nw, ne, se, sw) ->
    //             let closure' = buildClosure nw closure
    //             let closure'' = buildClosure ne closure'
    //             let closure''' = buildClosure se closure''
    //             buildClosure sw closure'''

    //     let closureMap = buildClosure qtree Map.empty

    //     closureMap |> Map.map (fun key _ -> findReachable qtree Set.empty)


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
                elif i > size / 2 && j <= size / 2 then 
                    findValue sw (size / 2) (i / 2) j
                elif i <= size / 2 && j > size / 2 then 
                    findValue ne (size / 2) i (j / 2)
                else 
                    findValue se (size / 2) (i / 2) (j / 2)

        let value = findValue graph.qtree graph.n i j
        value
        
    (*
    nw ne
    sw se
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
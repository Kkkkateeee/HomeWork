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
    let transitiveClosure (graph: Matrix<Edjes<'t>>) = 
        let toAdjacencyMatrix (graph: Matrix<Edjes<'t>>) =
            let toAdjacencyValue (edjes: Edjes<'t> option) =
                match edjes with
                | Some _ -> 1  // Есть ребро
                | None -> 0   // Нет ребра

            // Итеративный обход QTree
            let toAdjacencyQTreeIter (qtree: QTree<Edjes<'t>>) =
                QTrees.map (fun edjesOpt -> toAdjacencyValue edjesOpt) qtree
            
            { n = graph.n; qtree = toAdjacencyQTreeIter graph.qtree |> QTrees.map (fun x -> int x)}

        let valueReset (matrix: Matrix<int>) =
            let resetValue (value: int) = if value > 0 then 1 else 0
            let resetQTree (qtree:QTree<int>) = QTrees.map (fun x -> resetValue x ) qtree

            { n = matrix.n; qtree = resetQTree matrix.qtree |> QTrees.map(fun x -> int x) }

    // 4. Основная логика транзитивного замыкания:
        let adjM = toAdjacencyMatrix graph
        let mutable result = adjM

        for i in 2 .. graph.n do
        // Используем итеративное умножение матриц
            let power = Matrix.multiply result adjM (+) ( * )

        // valueReset и map2 объединены в одну операцию для эффективности
            result <- valueReset { n = graph.n; qtree = QTrees.map2 (+) result.qtree power.qtree }

        result

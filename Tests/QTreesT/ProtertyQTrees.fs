namespace PropertyQTrees 
 
open System 
open Xunit 
open FsCheck 
open FsCheck.Xunit 
open FsCheck.FSharp
 
open QTrees 
open QTrees.QTrees
 
 
module DataAndFuncs = 
 
    let areAlmostEqual (a: float32) (b: float32) (epsilon: float32) = 
        if Single.IsFinite(a) && Single.IsFinite(b) then abs (a - b) < epsilon  
        elif Single.IsNaN(a) && Single.IsNaN(b) then true 
        elif Single.IsInfinity(a) && Single.IsInfinity(b) then true 
        elif Single.IsNegativeInfinity(a) && Single.IsNegativeInfinity(b) then true 
        elif Single.IsPositiveInfinity(a) && Single.IsPositiveInfinity(b) then true 
        else false 
 
    let rec floatQTreesAreEqual (qtree1: QTree<float32>) (qtree2: QTree<float32>) : bool =  
        match qtree1, qtree2 with   
        | Node (nw, ne, se, sw), Node (NW, NE, SE, SW)  ->  
            floatQTreesAreEqual nw NW &&  
            floatQTreesAreEqual ne NE &&  
            floatQTreesAreEqual se SE &&  
            floatQTreesAreEqual sw SW 
        | Leaf (value1, size1), Leaf (value2, size2) ->  
            areAlmostEqual value1 value2 1e-5f 
        | _ -> false 

    let rec toCorrectQTree qtree  =
        match qtree with 
        | Leaf (value, size) -> Leaf (value, size)
        
        | Node (nw, ne, se, sw) ->
            let NW = toCorrectQTree nw
            let NE = toCorrectQTree ne
            let SE = toCorrectQTree se
            let SW = toCorrectQTree sw

            match NW, NE, SE, SW with 
            | Leaf (value1, size1), 
                Leaf (value2, size2), 
                Leaf (value3, size3), 
                Leaf (value4, size4) 
                when value1 = value2 && value2 = value3 && value3 = value4 ->
                
                Leaf (value1, size1 + size2)

            | _ -> Node (NW, NE, SE, SW)
 
    let rec high qtree = 
        match qtree with  
            | Leaf _-> 1 
            | Node (nw, ne, se, sw) -> 
                let heights = [| high nw; high ne; high se; high sw |] 
                Array.max heights + 1 


open DataAndFuncs 
 
[<Properties(MaxTest = 100)>] 
type Map() = 
     
 
    [<Property>] 
    member _.intId (qtree: QTree<int>) = 
        qtree |> toCorrectQTree = map id qtree 
 
    [<Property>] 
    member _.floatId (qtree: QTree<float32>) = 
        let res = map id qtree 
        Assert.True(floatQTreesAreEqual (qtree |> toCorrectQTree) res) 
         
 
    [<Property>] 
    member _.intComposition (qtree: QTree<int>) = 
        let qtree1 = map ((+) 1)((map ((+) 2)) qtree) 
        let qtree2 = map ((+) 3) qtree 
        qtree1 = qtree2 
 
    [<Property>] 
    member _.floatComposition (qtree: QTree<float32>) = 
        let qtree1 = map ((+) 1.0f) (map ((+) 2.0f) qtree) 
        let qtree2 = map ((+) 3.0f) qtree 
        Assert.True(floatQTreesAreEqual qtree1 qtree2) 
 
 
    [<Property>] 
    member _.intHighIsConst (qtree: QTree<int>) = 
        let h1 = high (qtree |> toCorrectQTree) 
        let h2 = high (map ((+) 1) qtree) 
        h1 = h2 
 
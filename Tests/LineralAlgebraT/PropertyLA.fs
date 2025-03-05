#nowarn "64"
   
namespace PropertyLA
 
open System 
open Xunit 
open FsCheck 
open FsCheck.Xunit 
open FsCheck.FSharp
 
open LineralAlgebra 
open LineralAlgebra.QTrees
open LineralAlgebra.Matrix
open UnitTrees.dataAndFuncs
 

module DataAndFuncs = 
    let rec floatQTreesAreEqual (qtree1: QTree<float32>) (qtree2: QTree<float32>) : bool =  
        match qtree1, qtree2 with   
        | Node (nw, ne, se, sw), Node (NW, NE, SE, SW)  ->  
            floatQTreesAreEqual nw NW &&  
            floatQTreesAreEqual ne NE &&  
            floatQTreesAreEqual se SE &&  
            floatQTreesAreEqual sw SW 
        | Leaf value1, Leaf value2 ->  
            areAlmostEqual value1 value2 1e-5f 
        | _ -> false 

    let rec toCorrectQTree qtree  =
        match qtree with 
        | Leaf value -> Leaf value
        
        | Node (nw, ne, se, sw) ->
            let NW = toCorrectQTree nw
            let NE = toCorrectQTree ne
            let SE = toCorrectQTree se
            let SW = toCorrectQTree sw

            match NW, NE, SE, SW with 
            |   Leaf value1, 
                Leaf value2, 
                Leaf value3, 
                Leaf value4 
                when value1 = value2 && value2 = value3 && value3 = value4 ->
                
                Leaf value1

            | _ -> Node (NW, NE, SE, SW)

    let multiplyArray2D (arr1: 't[,]) (arr2: 't[,]) : 't[,] =
        let rows1 = Array2D.length1 arr1
        let cols1 = Array2D.length2 arr1
        let rows2 = Array2D.length1 arr2
        let cols2 = Array2D.length2 arr2

        if cols1 <> rows2 then
            failwith "Number of columns in the first array must match the number of rows in the second array."

        let result = Array2D.create rows1 cols2 Unchecked.defaultof<'t>

        for i in 0 .. rows1 - 1 do   
            for j in 0 .. cols2 - 1 do 
                for k in 0 .. cols1 - 1 do
                    result.[i, j] <- result.[i, j] + arr1.[i, k] * arr2.[k, j]

        result

                
    let MatrixGen size typeOfMatr = 
        let rec QTreeGen size typeOfMatr  : QTree<int> =
            if size = 1 then 
                match typeOfMatr with
                | "Leaf" -> 
                    let value = Random().Next(-100, 101)
                    Leaf value
                | "Node" ->
                    let value = Random().Next(-100, 101)
                    Leaf value
                | "Zero" -> 
                    let value = 0
                    Leaf value
                | _ -> failwith "Not Implemented"
            else
                let nw = QTreeGen (size / 2) typeOfMatr
                let ne = QTreeGen (size / 2) typeOfMatr
                let sw = QTreeGen (size / 2) typeOfMatr
                let se = QTreeGen (size / 2) typeOfMatr
                Node (nw, ne, sw, se)

        let MatrGen (size: int) =
            let qtree = QTreeGen size typeOfMatr
            { n = size; qtree = toCorrectQTree qtree }

        let res = MatrGen size
        res


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
 

[<Properties(MaxTest = 100)>] 
type Multiply() = 

    let power = Random().Next(1, 6)
    let size = pown 2 power
         
    [<Property>]
    member _.leafLeaf () =
        let matr1 = MatrixGen 1 "Leaf"
        let matr2 = MatrixGen 1 "Leaf"
        let res = multiply matr1 matr2 (+) ( * )
        let arrayRess = qtreeToArray2D res.qtree res.n
        let arrayEx = multiplyArray2D (qtreeToArray2D matr1.qtree matr1.n) (qtreeToArray2D matr2.qtree matr2.n)
        Assert.Equal(arrayEx, arrayRess)

    [<Property>]
    member _.nodeLeaf () =
        let matr1 = MatrixGen size "Node"
        let matr2 = MatrixGen size "Leaf"
        let res = multiply matr1 matr2 (+) ( * )
        let arrayRess = qtreeToArray2D res.qtree res.n
        let arrayEx = multiplyArray2D (qtreeToArray2D matr1.qtree matr1.n) (qtreeToArray2D matr2.qtree matr2.n)
        Assert.Equal(arrayEx, arrayRess)

    [<Property>]
    member _.leafNode () =
        let matr1 = MatrixGen size "Leaf"
        let matr2 = MatrixGen size "Node"
        let res = multiply matr1 matr2 (+) ( * )
        let arrayRess = qtreeToArray2D res.qtree res.n
        let arrayEx = multiplyArray2D (qtreeToArray2D matr1.qtree matr1.n) (qtreeToArray2D matr2.qtree matr2.n)
        Assert.Equal(arrayEx, arrayRess)

    [<Property>]
    member _.nodeNode () =
        let matr1 = MatrixGen size "Node"
        let matr2 = MatrixGen size "Node"
        let res = multiply matr1 matr2 (+) ( * )
        let arrayRess = qtreeToArray2D res.qtree res.n
        let arrayEx = multiplyArray2D (qtreeToArray2D matr1.qtree matr1.n) (qtreeToArray2D matr2.qtree matr2.n)
        Assert.Equal(arrayEx, arrayRess)

    [<Property>]
    member _.Zero () =
        let matr1 = MatrixGen size "Zero"
        let matr2 = MatrixGen size "Node"
        let res = multiply matr1 matr2 (+) ( * )
        let arrayRess = qtreeToArray2D res.qtree res.n
        let res1 = multiply matr2 matr1 (+) ( * )
        let arrayRess1 = qtreeToArray2D res1.qtree res1.n
        
        let arrayEx = multiplyArray2D (qtreeToArray2D matr1.qtree matr1.n) (qtreeToArray2D matr2.qtree matr2.n)
        Assert.Equal(arrayEx, arrayRess)
        Assert.Equal(arrayEx, arrayRess1)
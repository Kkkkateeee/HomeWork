namespace UnetTrees

open Xunit 

open Trees
open Trees.tFunctions


module UnitTests =

    let Tree = 
        Node [
            Leaf 1
            Node [
                Leaf 2
                Leaf 3
            ]
            Leaf 4
        ]



    //[<Fact>]
    //let 
namespace UnitLA
 
open System 
open Xunit  
 
open LineralAlgebra 
 
 
module DataAndFuncs = 
 
    let intOneLeaf1 = Leaf 1
    let MintOneLeaf1 = { n = 4; qtree = intOneLeaf1 }

    let intOneLeaf2 = Leaf 2

    let intOneLeaf3 = Leaf 2
    let MintOneLeaf3 = { n = 1; qtree = intOneLeaf3 }

    let intOneLeaf4 = Leaf 3
    let MintOneLeaf4 = { n = 1; qtree = intOneLeaf4 }

    let intNode1 = Leaf 2
    let MintNode1 = { n = 2; qtree = intNode1 }

    let intNode2 = Leaf 3
    let MintNode2 = { n = 2; qtree = intNode2 }

    let intZero = Leaf 0
    let MintZero = { n = 4; qtree = intZero }

    let floatOneLeaf = Leaf 1.0
    
    let intQTree1 = Node ( 
        Node ( Leaf 1, Leaf 2, Leaf 3, Leaf 4), 
        Node ( Leaf -5, Leaf -6, Leaf -7, Leaf -8), 
        Leaf 9,  
        Node ( Leaf -10, Leaf -11, Leaf -12, Leaf -13)  
        ) 
    let MintQTree1 = { n = 4; qtree = intQTree1 }

    let intQTree2 = Node ( 
        Node ( Leaf 1, Leaf 2, Leaf 3, Leaf 4), 
        Leaf 5, 
        Leaf 6, 
        Node ( Leaf 7, Leaf 8, Leaf 9, Leaf 10)  
        ) 
    let MintQTree2 = { n = 4; qtree = intQTree2 }

    let floatQTree1 = Node ( 
        Node ( Leaf 1.0, Leaf 2.0, Leaf 3.0, Leaf 4.0), 
        Node ( Leaf -5.0, Leaf -6.0, Leaf -7.0, Leaf -8.0), 
        Leaf 9.0, 
        Node ( Leaf -10.0, Leaf -11.0, Leaf -12.0, Leaf -13.0)  
    ) 

    let floatQTree2 = Node ( 
        Node ( Leaf 1.0, Leaf 2.0, Leaf 3.0, Leaf 4.0), 
        Leaf 5.0, 
        Leaf 6.0, 
        Node ( Leaf 7.0, Leaf 8.0, Leaf 9.0, Leaf 10.0)  
    ) 

 
module Map = 
    open DataAndFuncs 
 
    [<Fact>] 
    let intId () = 
        let res = QTrees.map id intQTree1 
        Assert.Equal(intQTree1, res) 
 
    [<Fact>] 
    let floatId () = 
        let res = QTrees.map id floatQTree1 
        Assert.Equal(floatQTree1, res) 
  
 
    [<Fact>] 
    let intAdd () = 
        let res = QTrees.map ((+) 1) intQTree1 
        let ex = Node ( 
            Node ( Leaf 2, Leaf 3, Leaf 4, Leaf 5), 
            Node ( Leaf -4, Leaf -5, Leaf -6, Leaf -7), 
            Leaf 10, 
            Node ( Leaf -9, Leaf -10, Leaf -11, Leaf -12)  
        ) 
        Assert.Equal(ex, res) 

    [<Fact>] 
    let floatAdd () = 
        let res = QTrees.map ((+) 1.0) floatQTree1 
        let ex = Node ( 
            Node ( Leaf 2.0, Leaf 3.0, Leaf 4.0, Leaf 5.0), 
            Node ( Leaf -4.0, Leaf -5.0, Leaf -6.0, Leaf -7.0), 
            Leaf 10.0, 
            Node ( Leaf -9.0, Leaf -10.0, Leaf -11.0, Leaf -12.0)  
        ) 
        Assert.Equal(ex, res) 
 
 
    [<Fact>] 
    let intMult () = 
        let res = QTrees.map (fun x -> 2 * x) intQTree1 
        let ex = Node ( 
            Node ( Leaf 2, Leaf 4, Leaf 6, Leaf 8), 
            Node ( Leaf -10, Leaf -12, Leaf -14, Leaf -16), 
            Leaf 18, 
            Node ( Leaf -20, Leaf -22, Leaf -24, Leaf -26)   
        ) 
        Assert.Equal(ex, res) 
  
    [<Fact>] 
    let floatMult () = 
        let res = QTrees.map (fun x -> 2.0 * x) floatQTree1 
        let ex = Node ( 
            Node ( Leaf 2.0, Leaf 4.0, Leaf 6.0, Leaf 8.0), 
            Node ( Leaf -10.0, Leaf -12.0, Leaf -14.0, Leaf -16.0), 
            Leaf 18.0, 
            Node ( Leaf -20.0, Leaf -22.0, Leaf -24.0, Leaf -26.0)   
        ) 
        Assert.Equal(ex, res) 


    // functions intSign and floatSign are written to check that nodes with the same leaves are merged into 1 leaf

    [<Fact>] 
    let intSign () =
        let res = QTrees.map (fun (x: int) -> Math.Sign x) intQTree1
        let ex = Node (
            Leaf 1,
            Leaf -1,
            Leaf 1,
            Leaf -1
        )
        Assert.Equal(ex, res)

    [<Fact>] 
    let floatSign () =
        let res = QTrees.map (fun (x: float) -> Math.Sign x) floatQTree1
        let ex = Node (
            Leaf 1,
            Leaf -1,
            Leaf 1,
            Leaf -1
        )
        Assert.Equal(ex, res)
 
 
    [<Fact>] 
    let intComposition () = 
        let tree1 = QTrees.map ((+) 1)((QTrees.map ((+) 2)) intQTree1) 
        let tree2 = QTrees.map ((+) 3) intQTree1 
        let ex = Node ( 
            Node ( Leaf 4, Leaf 5, Leaf 6, Leaf 7), 
            Node ( Leaf -2, Leaf -3, Leaf -4, Leaf -5), 
            Leaf 12, 
            Node ( Leaf -7, Leaf -8, Leaf -9, Leaf -10)  
        ) 
        Assert.Equal(ex, tree1) 
        Assert.Equal(ex, tree2) 

    [<Fact>] 
    let floatComposition () = 
        let tree1 = QTrees.map ((+) 1.0) (QTrees.map ((+) 2.0) floatQTree1) 
        let tree2 = QTrees.map ((+) 3.0) floatQTree1 
        let ex = Node ( 
            Node ( Leaf 4.0, Leaf 5.0, Leaf 6.0, Leaf 7.0), 
            Node ( Leaf -2.0, Leaf -3.0, Leaf -4.0, Leaf -5.0), 
            Leaf 12.0, 
            Node ( Leaf -7, Leaf -8.0, Leaf -9.0, Leaf -10.0)  
        )
        Assert.Equal(ex, tree1) 
        Assert.Equal(ex, tree2) 
 

    [<Fact>] 
    let intHighIsConst () = 
        let h1 = QTrees.height intQTree1 
        let h2 = QTrees.height (QTrees.map ((+) 1) intQTree1) 
        Assert.Equal(3, h1) 
        Assert.Equal(3, h2) 
 
    [<Fact>] 
    let floatHighIsConst () = 
        let h1 = QTrees.height floatQTree1 
        let h2 = QTrees.height (QTrees.map ((+) 1.0) floatQTree1) 
        Assert.Equal(3, h1) 
        Assert.Equal(3, h2) 


    (*
    functions intHighIsExpected and floatHighIsExpected are written to check that 
    when merging nodes into a leaf, the height decreases correctly
    *)

    [<Fact>] 
    let intHighIsExpected () = 
        let h = QTrees.height (QTrees.map (fun (x: int) -> Math.Sign x) intQTree1) 
        Assert.Equal(2, h) 
 
    [<Fact>]
    let floatHighIsExpected () = 
        let h = QTrees.height (QTrees.map (fun (x: float) -> Math.Sign x) floatQTree1) 
        Assert.Equal(2, h) 

    
module Map2 = 
    open DataAndFuncs 
 
    [<Fact>] 
    let intAdd () = 
        let res = QTrees.map2 (+) intQTree1 intQTree2 
        let ex = Node ( 
            Node ( Leaf 2, Leaf 4, Leaf 6, Leaf 8), 
            Node ( Leaf 0, Leaf -1, Leaf -2, Leaf -3), 
            Leaf 15, 
            Leaf -3
            ) 
        Assert.Equal(ex, res) 

    [<Fact>] 
    let floatAdd () = 
        let res = QTrees.map2 (+) floatQTree1 floatQTree2 
        let ex = Node ( 
            Node ( Leaf 2.0, Leaf 4.0, Leaf 6.0, Leaf 8.0), 
            Node ( Leaf 0.0, Leaf -1.0, Leaf -2.0, Leaf -3.0), 
            Leaf 15.0, 
            Leaf -3.0  
            ) 
        Assert.Equal(ex, res) 
 
 
    [<Fact>] 
    let intMult () = 
        let res = QTrees.map2 (fun x y -> x * y) intQTree1 intQTree2 
        let ex = Node ( 
            Node ( Leaf 1, Leaf 4, Leaf 9, Leaf 16), 
            Node ( Leaf -25, Leaf -30, Leaf -35, Leaf -40), 
            Leaf 54, 
            Node ( Leaf -70, Leaf -88, Leaf -108, Leaf -130)   
            ) 
        Assert.Equal(ex, res) 

    [<Fact>] 
    let floatMult () = 
        let res = QTrees.map2 (fun x y -> x * y) floatQTree1 floatQTree2 
        let ex = Node ( 
            Node ( Leaf 1.0, Leaf 4.0, Leaf 9.0, Leaf 16.0), 
            Node ( Leaf -25.0, Leaf -30.0, Leaf -35.0, Leaf -40.0), 
            Leaf 54.0, 
            Node ( Leaf -70.0, Leaf -88.0, Leaf -108.0, Leaf -130.0)   
            ) 
        Assert.Equal(ex, res) 
 
 
    [<Fact>] 
    let intHighIsExpected () = 
        let h1 = QTrees.height intQTree1 
        let h2 = QTrees.height intOneLeaf1 
        let h3 = max h1 h2 
        let h4 = QTrees.height (QTrees.map2 (+) intQTree1 intOneLeaf1) 
        Assert.Equal(3, h3) 
        Assert.Equal(3, h4) 
     
    [<Fact>] 
    let floatHighIsExpected () = 
        let h1 = QTrees.height floatQTree1 
        let h2 = QTrees.height floatOneLeaf 
        let h3 = max h1 h2 
        let h4 = QTrees.height (QTrees.map2 (+) floatQTree1 floatOneLeaf) 
        Assert.Equal(3, h3) 
        Assert.Equal(3, h4) 
 
 
module Multiply = 
    open DataAndFuncs

    [<Fact>]
    let sizeIsImportant () =
        let res1 = Matrix.multiply MintOneLeaf3 MintOneLeaf4 (+) ( * ) 0
        let ex1 = { n = 1; qtree = Leaf 6 }
        let res2 = Matrix.multiply MintNode1 MintNode2 (+) ( * ) 0
        let ex2 = { n = 2; qtree = Leaf 12 }
        Assert.Equal(ex1, res1)
        Assert.Equal(ex2, res2)
    
    [<Fact>]
    let leafLeaf () =
        let res = Matrix.multiply MintOneLeaf3 MintOneLeaf4 (+) ( * ) 0
        let ex = { n = 1; qtree = Leaf 6 }
        Assert.Equal(ex, res)

    [<Fact>]
    let nodeLeaf () =
        let res = Matrix.multiply MintQTree1 MintOneLeaf1 (+) ( * ) 0
        let ex = { 
            n = 4; 
            qtree = Node ( 
                Leaf -8, 
                Leaf -8, 
                Node ( Leaf -3, Leaf -3, Leaf -7, Leaf -7), 
                Node ( Leaf -3, Leaf -3, Leaf -7, Leaf -7)    
            )  
        }
        Assert.Equal(ex, res)

    [<Fact>]
    let leafNode () =
        let res = Matrix.multiply MintOneLeaf1 MintQTree1 (+) ( * ) 0
        let ex = { 
            n = 4; 
            qtree = Node ( 
                Node ( Leaf 22, Leaf 24, Leaf 22, Leaf 24), 
                Node ( Leaf -34, Leaf -38, Leaf -34, Leaf -38), 
                Node ( Leaf 22, Leaf 24, Leaf 22, Leaf 24), 
                Node ( Leaf -34, Leaf -38, Leaf -34, Leaf -38)  
            ) 
        }
        Assert.Equal(ex, res)

    [<Fact>]
    let nodeNode () =
        let res = Matrix.multiply MintQTree1 MintQTree2 (+) ( * ) 0
        let ex = { 
            n = 4; 
            qtree = Node ( 
                Node (Leaf -59, Leaf -56, Leaf -75, Leaf -68),
                Node (Leaf -74, Leaf -85, Leaf -86, Leaf -101),
                Node (Leaf -90, Leaf -72, Leaf -114, Leaf -96),
                Node (Leaf -79, Leaf -100, Leaf -111, Leaf -136) 
            ) 
        }
        Assert.Equal(ex, res)

    [<Fact>]
    let zero () =
        let res1 = Matrix.multiply MintQTree1 MintZero (+) ( * ) 0
        let res2 = Matrix.multiply MintZero MintQTree1 (+) ( * ) 0
        let ex = { n = 4; qtree = Leaf 0 }
        Assert.Equal(ex, res1)
        Assert.Equal(ex, res2)
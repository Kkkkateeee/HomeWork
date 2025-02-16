namespace UnitQTrees 
 
open System 
open Xunit  
 
open QTrees 
open QTrees.QTrees
 
 
module DataAndFuncs = 
 
    let intOneLeaf1 = Leaf (1, 1)
    let intOneLeaf2 = Leaf (2, 1)
    let intOneLeaf3 = Leaf (2, 1)
    let intOneLeaf4 = Leaf (3, 1)
    let intNode1 = Leaf (2, 2)
    let intNode2 = Leaf (3, 2)
    let intZero = Leaf (0, 4)
    let floatOneLeaf = Leaf (1.0, 1) 

    let intQTree1 = Node ( 
        Node ( Leaf (1, 1), Leaf (2, 1), Leaf (3, 1), Leaf (4, 1)), 
        Node ( Leaf (5, 1), Leaf (6, 1), Leaf (7, 1), Leaf (8, 1)), 
        Leaf (9, 2),  
        Node ( Leaf (10, 1), Leaf (11, 1), Leaf (12, 1), Leaf (13, 1))  
        ) 
 
    let intQTree2 = Node ( 
        Node ( Leaf (1, 1), Leaf (2, 1), Leaf (3, 1), Leaf (4, 1)), 
        Leaf (5, 2), 
        Leaf (6, 2), 
        Node ( Leaf (7, 1), Leaf (8, 1), Leaf (9, 1), Leaf (10, 1))  
        ) 

    let floatQTree1 = Node ( 
        Node ( Leaf (1.0, 1), Leaf (2.0, 1), Leaf (3.0, 1), Leaf (4.0, 1)), 
        Node ( Leaf (5.0, 1), Leaf (6.0, 1), Leaf (7.0, 1), Leaf (8.0, 1)), 
        Leaf (9.0, 2), 
        Node ( Leaf (10.0, 1), Leaf (11.0, 1), Leaf (12.0, 1), Leaf (13.0, 1))  
    ) 

    let floatQTree2 = Node ( 
        Node ( Leaf (1.0, 1), Leaf (2.0, 1), Leaf (3.0, 1), Leaf (4.0, 1)), 
        Leaf (5.0, 2), 
        Leaf (6.0, 2), 
        Node ( Leaf (7.0, 1), Leaf (8.0, 1), Leaf (9.0, 1), Leaf (10.0, 1))  
    ) 

    let intQTSign = Node (
        Node ( Leaf (1, 1), Leaf (2, 1), Leaf (3, 1), Leaf (4, 1)), 
        Node ( Leaf (-5, 1), Leaf (-6, 1), Leaf (-7, 1), Leaf (-8, 1)), 
        Leaf (9, 2),  
        Node ( Leaf (-10, 1), Leaf (-11, 1), Leaf (-12, 1), Leaf (-13, 1))
    )

    let floatQTSign = Node (
        Node ( Leaf (1.0, 1), Leaf (2.0, 1), Leaf (3.0, 1), Leaf (4.0, 1)), 
        Node ( Leaf (-5.0, 1), Leaf (-6.0, 1), Leaf (-7.0, 1), Leaf (-8.0, 1)), 
        Leaf (9.0, 2),  
        Node ( Leaf (-10.0, 1), Leaf (-11.0, 1), Leaf (-12.0, 1), Leaf (-13.0, 1))
    )
 

    let rec high qtree = 
        match qtree with  
            | Leaf _ -> 1 
            | Node (nw, ne, se, sw) -> 
                let heights = [| high nw; high ne; high se; high sw |] 
                Array.max heights + 1 

 
module Map = 
    open DataAndFuncs 
 
    [<Fact>] 
    let intId () = 
        let res = map id intQTree1 
        Assert.Equal(intQTree1, res) 
 
    [<Fact>] 
    let floatId () = 
        let res = map id floatQTree1 
        Assert.Equal(floatQTree1, res) 
  
 
    [<Fact>] 
    let intAdd () = 
        let res = map ((+) 1) intQTree1 
        let ex = Node ( 
            Node ( Leaf (2, 1), Leaf (3, 1), Leaf (4, 1), Leaf (5, 1)), 
            Node ( Leaf (6, 1), Leaf (7, 1), Leaf (8, 1), Leaf (9, 1)), 
            Leaf (10, 2), 
            Node ( Leaf (11, 1), Leaf (12, 1), Leaf (13, 1), Leaf (14, 1))  
        ) 
        Assert.Equal(ex, res) 
 
    [<Fact>] 
    let floatAdd () = 
        let res = map ((+) 1.0) floatQTree1 
        let ex = Node ( 
            Node ( Leaf (2.0, 1), Leaf (3.0, 1), Leaf (4.0, 1), Leaf (5.0, 1)), 
            Node ( Leaf (6.0, 1), Leaf (7.0, 1), Leaf (8.0, 1), Leaf (9.0, 1)), 
            Leaf (10.0, 2), 
            Node ( Leaf (11.0, 1), Leaf (12.0, 1), Leaf (13.0, 1), Leaf (14.0, 1))  
        ) 
        Assert.Equal(ex, res) 
 
 
    [<Fact>] 
    let intMult () = 
        let res = map (fun x -> 2 * x) intQTree1 
        let ex = Node ( 
            Node ( Leaf (2, 1), Leaf (4, 1), Leaf (6, 1), Leaf (8, 1)), 
            Node ( Leaf (10, 1), Leaf (12, 1), Leaf (14, 1), Leaf (16, 1)), 
            Leaf (18, 2), 
            Node ( Leaf (20, 1), Leaf (22, 1), Leaf (24, 1), Leaf (26, 1))   
        ) 
        Assert.Equal(ex, res) 
 
    [<Fact>] 
    let floatMult () = 
        let res = map (fun x -> 2.0 * x) floatQTree1 
        let ex = Node ( 
            Node ( Leaf (2.0, 1), Leaf (4.0, 1), Leaf (6.0, 1), Leaf (8.0, 1)), 
            Node ( Leaf (10.0, 1), Leaf (12.0, 1), Leaf (14.0, 1), Leaf (16.0, 1)), 
            Leaf (18.0, 2), 
            Node ( Leaf (20.0, 1), Leaf (22.0, 1), Leaf (24.0, 1), Leaf (26.0, 1))   
        ) 
        Assert.Equal(ex, res) 


    // functions intSign and floatSign are written to check that nodes with the same leaves are merged into 1 leaf

    [<Fact>] 
    let intSign () =
        let res = map (fun (x: int) -> Math.Sign x) intQTSign
        let ex = Node (
            Leaf (1, 2),
            Leaf (-1, 2),
            Leaf (1, 2),
            Leaf (-1, 2)
        )
        Assert.Equal(ex, res)

    [<Fact>] 
    let floatSign () =
        let res = map (fun (x: float) -> Math.Sign x) floatQTSign
        let ex = Node (
            Leaf (1, 2),
            Leaf (-1, 2),
            Leaf (1, 2),
            Leaf (-1, 2)
        )
        Assert.Equal(ex, res)
 
 
    [<Fact>] 
    let intComposition () = 
        let tree1 = map ((+) 1)((map ((+) 2)) intQTree1) 
        let tree2 = map ((+) 3) intQTree1 
        let ex = Node ( 
            Node ( Leaf (4, 1), Leaf (5, 1), Leaf (6, 1), Leaf (7, 1)), 
            Node ( Leaf (8, 1), Leaf (9, 1), Leaf (10, 1), Leaf (11, 1)), 
            Leaf (12, 2), 
            Node ( Leaf (13, 1), Leaf (14, 1), Leaf (15, 1), Leaf (16, 1))  
        ) 
        Assert.Equal(ex, tree1) 
        Assert.Equal(ex, tree2) 
 
    [<Fact>] 
    let floatComposition () = 
        let tree1 = map ((+) 1.0) (map ((+) 2.0) floatQTree1) 
        let tree2 = map ((+) 3.0) floatQTree1 
        let ex = Node ( 
            Node ( Leaf (4.0, 1), Leaf (5.0, 1), Leaf (6.0, 1), Leaf (7.0, 1)), 
            Node ( Leaf (8.0, 1), Leaf (9.0, 1), Leaf (10.0, 1), Leaf (11.0, 1)), 
            Leaf (12.0, 2), 
            Node ( Leaf (13.0, 1), Leaf (14.0, 1), Leaf (15.0, 1), Leaf (16.0, 1))  
        )
        Assert.Equal(ex, tree1) 
        Assert.Equal(ex, tree2) 
 

    [<Fact>] 
    let intHighIsConst () = 
        let h1 = high intQTree1 
        let h2 = high (map ((+) 1) intQTree1) 
        Assert.Equal(3, h1) 
        Assert.Equal(3, h2) 
 
    [<Fact>] 
    let floatHighIsConst () = 
        let h1 = high floatQTree1 
        let h2 = high (map ((+) 1.0) floatQTree1) 
        Assert.Equal(3, h1) 
        Assert.Equal(3, h2) 


    (*
    functions intHighIsExpected and floatHighIsExpected are written to check that 
    when merging nodes into a leaf, the height decreases correctly
    *)

    [<Fact>] 
    let intHighIsExpected () = 
        let h = high (map (fun (x: int) -> Math.Sign x) intQTSign) 
        Assert.Equal(2, h) 
 
    [<Fact>] 
    let floatHighIsExpected () = 
        let h = high (map (fun (x: float) -> Math.Sign x) floatQTSign) 
        Assert.Equal(2, h) 

 
module Map2 = 
    open DataAndFuncs 
 
    [<Fact>] 
    let intAdd () = 
        let res = map2 (+) intQTree1 intQTree2 
        let ex = Node ( 
            Node ( Leaf (2, 1), Leaf (4, 1), Leaf (6, 1), Leaf (8, 1)), 
            Node ( Leaf (10, 1), Leaf (11, 1), Leaf (12, 1), Leaf (13, 1)), 
            Leaf (15, 2), 
            Node ( Leaf (17, 1), Leaf (19, 1), Leaf (21, 1), Leaf (23, 1))  
            ) 
        Assert.Equal(ex, res) 
 
    [<Fact>] 
    let floatAdd () = 
        let res = map2 (+) floatQTree1 floatQTree2 
        let ex = Node ( 
            Node ( Leaf (2.0, 1), Leaf (4.0, 1), Leaf (6.0, 1), Leaf (8.0, 1)), 
            Node ( Leaf (10.0, 1), Leaf (11.0, 1), Leaf (12.0, 1), Leaf (13.0, 1)), 
            Leaf (15.0, 2), 
            Node ( Leaf (17.0, 1), Leaf (19.0, 1), Leaf (21.0, 1), Leaf (23.0, 1))  
            ) 
        Assert.Equal(ex, res) 
 
 
    [<Fact>] 
    let intMult () = 
        let res = map2 (fun x y -> x * y) intQTree1 intQTree2 
        let ex = Node ( 
            Node ( Leaf (1, 1), Leaf (4, 1), Leaf (9, 1), Leaf (16, 1)), 
            Node ( Leaf (25, 1), Leaf (30, 1), Leaf (35, 1), Leaf (40, 1)), 
            Leaf (54, 2), 
            Node ( Leaf (70, 1), Leaf (88, 1), Leaf (108, 1), Leaf (130, 1))   
            ) 
        Assert.Equal(ex, res) 
 
    [<Fact>] 
    let floatMult () = 
        let res = map2 (fun x y -> x * y) floatQTree1 floatQTree2 
        let ex = Node ( 
            Node ( Leaf (1.0, 1), Leaf (4.0, 1), Leaf (9.0, 1), Leaf (16.0, 1)), 
            Node ( Leaf (25.0, 1), Leaf (30.0, 1), Leaf (35.0, 1), Leaf (40.0, 1)), 
            Leaf (54.0, 2), 
            Node ( Leaf (70.0, 1), Leaf (88.0, 1), Leaf (108.0, 1), Leaf (130.0, 1))   
            ) 
        Assert.Equal(ex, res) 
 
 
    [<Fact>] 
    let intHighIsExpected () = 
        let h1 = high intQTree1 
        let h2 = high intOneLeaf1 
        let h3 = max h1 h2 
        let h4 = high (map2 (+) intQTree1 intOneLeaf1) 
        Assert.Equal(3, h3) 
        Assert.Equal(3, h4) 
     
    [<Fact>] 
    let floatHighIsExpected () = 
        let h1 = high floatQTree1 
        let h2 = high floatOneLeaf 
        let h3 = max h1 h2 
        let h4 = high (map2 (+) floatQTree1 floatOneLeaf) 
        Assert.Equal(3, h3) 
        Assert.Equal(3, h4) 
 
 
module Mult = 
    open DataAndFuncs

    [<Fact>]
    let sizeIsImportant () =
        let res1 = multiply intOneLeaf3 intOneLeaf4 (+) (fun x y -> x * y)
        let ex1 = Leaf (6, 1)
        let res2 = multiply intNode1 intNode2 (+) (fun x y -> x * y)
        let ex2 = Leaf (12, 2)
        Assert.Equal(ex1, res1)
        Assert.Equal(ex2, res2)
    
    [<Fact>]
    let leafLeaf () =
        let res = multiply intOneLeaf1 intOneLeaf2 (+) (fun x y -> x * y)
        let ex = Leaf (2, 1)
        Assert.Equal(ex, res)

    [<Fact>]
    let nodeLeaf () =
        let res = multiply intQTree1 intOneLeaf1 (+) (fun x y -> x * y)
        let ex = Node ( 
            Node ( Leaf (14, 1), Leaf (14, 1), Leaf (22, 1), Leaf (22, 1)), 
            Node ( Leaf (14, 1), Leaf (14, 1), Leaf (22, 1), Leaf (22, 1)), 
            Node ( Leaf (39, 1), Leaf (39, 1), Leaf (43, 1), Leaf (43, 1)), 
            Node ( Leaf (39, 1), Leaf (39, 1), Leaf (43, 1), Leaf (43, 1))   
            ) 
        Assert.Equal(ex, res)

    [<Fact>]
    let leafNode () =
        let res = multiply intOneLeaf1 intQTree1 (+) (fun x y -> x * y)
        let ex = Node ( 
            Leaf (28, 2), 
            Leaf (31, 2), 
            Leaf (31, 2), 
            Leaf (28, 2)
            ) 
        Assert.Equal(ex, res)

    [<Fact>]
    let nodeNode () =
        let res = multiply intQTree1 intQTree2 (+) (fun x y -> x * y)
        let ex = Node ( 
            Node ( Leaf (104, 1), Leaf (102, 1), Leaf (144, 1), Leaf (142, 1)), 
            Node ( Leaf (81, 1),Leaf (81, 1), Leaf (125, 1), Leaf (125, 1)), 
            Node ( Leaf (213, 1), Leaf (213, 1), Leaf (233, 1), Leaf (233, 1)), 
            Node ( Leaf (207, 1), Leaf (206, 1), Leaf (215, 1), Leaf (214, 1))   
            ) 
        Assert.Equal(ex, res)


    [<Fact>]
    let intZero () =
        let res1 = multiply intQTree1 intZero (+) (fun x y -> x * y) 
        let res2 = multiply intZero intQTree1 (+) (fun x y -> x * y) 
        let ex = Leaf (0, 4)
        Assert.Equal(ex, res1)
        Assert.Equal(ex, res2)
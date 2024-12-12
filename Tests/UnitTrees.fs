namespace UnitTrees

open System
open Xunit 

open Trees
open Trees.Tree

module dataAndFuncs =

    let oneLeaf = Leaf 1

    let intTree = 
        Node(Cons(
                Node(Cons(
                        Leaf(1), 
                        Single (Leaf(2)))
                    ), 
                Single (Node(Single (Leaf(3))))
                )
            )

    let floatTree = 
        Node(Cons(
                Node(Cons(
                        Leaf(1.00f), 
                        Single (Leaf(2.00f)))
                    ), 
                Single (Node(Single (Leaf(3.00f))))
                )
            )

    let charTree = 
        Node(Cons(
                Node(Cons(
                        Leaf('a'), 
                        Single (Leaf('b')))
                    ), 
                Single (Node(Single (Leaf('c'))))
                )
            )

    let arrayTree = 
        Node(Cons(
                Node(Cons(
                        Leaf([|1; 2; 3|]), 
                        Single (Leaf([|5; 0; 0|])))
                    ), 
                Single (Node(Single (Leaf([|2; 3|]))))
                )
            )

    let rec leafToList tree =
        match tree with 
        | Leaf value -> [value]
        | Node children ->
            match children with
            | Single child -> leafToList child
            | Cons (head, tail) -> 
                leafToList head @ leafToList (Node tail)

    let areAlmostEqual (a: float32) (b: float32) (epsilon: float32) =
        if Single.IsFinite(a) && Single.IsFinite(b) then abs (a - b) < epsilon 
        elif Single.IsNaN(a) && Single.IsNaN(b) then true
        elif Single.IsInfinity(a) && Single.IsInfinity(b) then true
        elif Single.IsNegativeInfinity(a) && Single.IsNegativeInfinity(b) then true
        elif Single.IsPositiveInfinity(a) && Single.IsPositiveInfinity(b) then true
        else false
            

module map =
    open dataAndFuncs

    [<Fact>]
    let intId () = intTree = map id intTree
    
    [<Fact>]
    let floatId () = floatTree = map id floatTree

    [<Fact>]
    let charId () = charTree = map id charTree

    [<Fact>]
    let arrayId () = arrayTree = map id arrayTree


    [<Fact>]
    let intListEqTree () = 
        let list = List.map ((+) 1) (leafToList intTree)
        let treeList = leafToList (map ((+) 1) intTree)
        let ex =  [ 2; 3; 4 ]
        Assert.Equal<int>(ex, list)
        Assert.Equal<int>(ex, treeList)
        
    [<Fact>]
    let floatListEqTree () = 
        let list = List.map ((+) 1.0f) (leafToList floatTree)
        let treeList = leafToList (map (fun x -> x + 1.0f) floatTree)
        let ex =  [ 2.00f; 3.00f; 4.00f ]
        Assert.Equal<float32>(ex, list)
        Assert.Equal<float32>(ex, treeList)

    [<Fact>]
    let charListEqTree () = 
        let list = List.map (char << ((+) 1 << int)) (leafToList charTree)
        let treeList = leafToList (map (fun c -> char (int c + 1)) charTree)
        let ex =  [ 'b'; 'c'; 'd' ]
        Assert.Equal<char>(ex, list)
        Assert.Equal<char>(ex, treeList)

    [<Fact>]
    let arrayListEqTree () = 
        let func list = Array.map (fun x -> x * x) list
        let list = List.map func (leafToList arrayTree)
        let treeList = leafToList (map func arrayTree)
        let ex =  [ [|1; 4; 9|]; [|25; 0; 0|]; [|4; 9|] ]
        Assert.Equal<array<int>>(ex, list)
        Assert.Equal<array<int>>(ex, treeList)


    [<Fact>]
    let intComposition () =
        let tree1 = map ((+) 2) (map ((+) 1) intTree)
        let tree2 = map ((+) 3) intTree
        let ex = Node(Cons(Node(Cons(Leaf(4), Single (Leaf(5)))), Single (Node(Single (Leaf(6))))))
        Assert.Equal<Tree<int>>(ex, tree1)
        Assert.Equal<Tree<int>>(ex, tree2)

    [<Fact>]  
    let floatComposition () =
        let tree1 = leafToList (map ((+) 1.00f) (map ((+) 2.00f) floatTree))
        let tree2 = leafToList (map ((+) 3.00f) floatTree)
        let ex = leafToList (Node(Cons(Node(Cons(Leaf(4.00f), Single (Leaf(5.00f)))), Single (Node(Single (Leaf(6.00f)))))))
        Assert.True(List.forall2 (fun e a -> areAlmostEqual e a 1e-10f) ex tree1)
        Assert.True(List.forall2 (fun e a -> areAlmostEqual e a 1e-10f) ex tree2)

    [<Fact>]
    let charComposition () =
        let tree1 = map (char << ((+) 1 << int)) (map (char << ((+) 1 << int)) charTree)
        let tree2 = map (char << ((+) 2 << int)) charTree
        let ex = Node(Cons(Node(Cons(Leaf('c'), Single (Leaf('d')))), Single (Node(Single (Leaf('e'))))))
        Assert.Equal<Tree<char>>(ex, tree1)
        Assert.Equal<Tree<char>>(ex, tree2)

    [<Fact>]
    let arrayComposition () =
        let f list a = Array.map (fun x -> a * x) list 
        let tree1 = map (fun a -> f a 3) (map (fun a -> f a 2) arrayTree)
        let tree2 = map (fun a -> f a 6) arrayTree
        let ex = Node(Cons(Node(Cons(Leaf([|6; 12; 18|]), Single (Leaf([|30; 0; 0|])))), Single (Node(Single (Leaf([|12; 18|]))))))
        Assert.Equal<Tree<array<int>>>(ex, tree1)
        Assert.Equal<Tree<array<int>>>(ex, tree2)


    [<Fact>]
    let intHighIsConst () =
        let h = high (map ((+) 3) intTree) 
        Assert.Equal<int>(3, h)

    [<Fact>]
    let floatHighIsConst () =
        let h = high (map ((+) 3.0f) floatTree)
        Assert.Equal<int>(3, h)

    [<Fact>]
    let charHighIsConst () =
        let h = high (map (char << ((+) 1 << int)) charTree)
        Assert.Equal<int>(3, h)

    [<Fact>]
    let arrayHighIsConst () =
        let f a list = Array.map (fun x -> a * x) list
        let h = high (map (fun a -> f 2 ) arrayTree)
        Assert.Equal<int>(3, h)

    
module AssociativeFolds =
    open dataAndFuncs

    [<Fact>]
    let intAdd () =
        let sumFunc value acc = acc + value
        let sum1 = foldLeft sumFunc 0 intTree
        let sum2 = foldRight sumFunc 0 intTree
        Assert.Equal<int>(6, sum1)
        Assert.Equal<int>(6, sum2)

    [<Fact>]
    let floatAdd () =
        let sumFunc acc value = acc + value
        let sum1 = foldLeft sumFunc 0.0f floatTree
        let sum2 = foldRight sumFunc 0.0f floatTree
        Assert.Equal(6.00f, sum1, 1)
        Assert.Equal(6.00f, sum2, 1)
        
    [<Fact>]
    let charAdd () =
        let sumFunc acc value = acc + (int value)
        let sum1 = foldLeft sumFunc 0 charTree
        let sum2 = foldRight sumFunc 0 charTree
        Assert.Equal<int>(294, sum1)
        Assert.Equal<int>(294, sum2)


    [<Fact>]
    let intMult () =
        let sumFunc acc value = acc * value
        let sum1 = foldLeft sumFunc 1 intTree
        let sum2 = foldRight sumFunc 1 intTree
        Assert.Equal<int>(6, sum1)
        Assert.Equal<int>(6, sum2)

    [<Fact>]
    let floatMult () =
        let sumFunc acc value = acc * value
        let sum1 = foldLeft sumFunc 1.0f floatTree
        let sum2 = foldRight sumFunc 1.0f floatTree
        Assert.Equal(6.00f, sum1, 1)
        Assert.Equal(6.00f, sum2, 1)

    [<Fact>]
    let charMult () =
        let sumFunc acc value = acc * (int value)
        let sum1 = foldLeft sumFunc 1 charTree
        let sum2 = foldRight sumFunc 1 charTree
        Assert.Equal<int>(941094, sum1)
        Assert.Equal<int>(941094, sum2)
   
   
module tHighOfTree =
    open dataAndFuncs

    [<Fact>]
    let high () =
        let oneHigh = high oneLeaf
        let intHigh = high intTree
        let floatHigh = high floatTree
        let charHigh = high charTree
        let arrayHigh = high arrayTree

        Assert.Equal(1, oneHigh)
        Assert.Equal(3, intHigh)
        Assert.Equal(3, floatHigh)
        Assert.Equal(3, charHigh)
        Assert.Equal(3, arrayHigh)

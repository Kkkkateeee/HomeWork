namespace UnetTrees

open System
open Xunit 

open Trees
open Trees.Tree

module dataAndFuncs =

    let intTree = 
        Node [
            Leaf 1
            Node [
                Leaf 2
                Leaf 3
            ]
            Leaf 4
        ]

    let floatTree =
        Node [
            Node[
                Leaf 6.77
                Leaf 0.111
            ]
            Leaf 1.333
        ]

    let charTree =
        Node [
            Node[
                Leaf 'a'
                Leaf 'r'
            ]
            Leaf 'a'
        ]

    let stringTree =
        Node [
            Node[
                Leaf "Hello"
                Leaf "World"
                Leaf "!"
            ]
            Leaf "f$harp"
        ]

    let boolTree =
        Node [
            Node[
                Leaf true
                Leaf true
            ]
            Leaf false
        ]

    let listTree =
        Node [
            Node[
                Leaf [1; 2; 3]
                Leaf [5; 0; 0]
            ]
            Leaf [2; 3]
        ]

    let arrayTree =
        Node [
            Node[
                Leaf [|1; 2; 3|]
                Leaf [|5; 0; 0|]
            ]
            Leaf [|2; 3|]
        ]


    let rec leafToList tree =
        match tree with 
        | Leaf value -> [value]
        | Node children -> 
            List.collect leafToList children 

    let areAlmostEqual (a: float) (b: float) (epsilon: float) =
        abs (a - b) < epsilon


module map =
    open dataAndFuncs

    [<Fact>]
    let intId () = intTree = map id intTree
    
    [<Fact>]
    let floatId () = floatTree = map id floatTree

    [<Fact>]
    let charId () = charTree = map id charTree

    [<Fact>]
    let stringId () = stringTree = map id stringTree

    [<Fact>]
    let boolId () = boolTree = map id boolTree

    [<Fact>]
    let listId () = listTree = map id listTree

    [<Fact>]
    let arrayId () = arrayTree = map id arrayTree


    [<Fact>]
    let intListEqTree () = 
        let list = List.map ((+) 1 ) (leafToList intTree)
        let treeList = leafToList (map (fun x -> x + 1) intTree)
        let expected =  [ 2; 3; 4; 5 ]
        Assert.Equal<int>(expected, list)
        Assert.Equal<int>(expected, treeList)
        
    [<Fact>]
    let floatListEqTree () = 
        let list = List.map ((+) 1.0 ) (leafToList floatTree)
        let treeList = leafToList (map (fun x -> x + 1.0) floatTree)
        let expected =  [ 7.77; 1.111; 2.333 ]
        Assert.Equal<float>(expected, list)
        Assert.Equal<float>(expected, treeList)

    [<Fact>]
    let charListEqTree () = 
        let list = List.map (fun c -> char (int c + 1)) (leafToList charTree)
        let treeList = leafToList (map (fun c -> char (int c + 1)) charTree)
        let expected =  [ 'b'; 's'; 'b' ]
        Assert.Equal<char>(expected, list)
        Assert.Equal<char>(expected, treeList)

    [<Fact>]
    let stringListEqTree () = 
        let list = List.map (fun s -> s + "!") (leafToList stringTree)
        let treeList = leafToList (map (fun s -> s + "!") stringTree)
        let expected =  ["Hello!"; "World!"; "!!"; "f$harp!" ]
        Assert.Equal<string>(expected, list)
        Assert.Equal<string>(expected, treeList)

    [<Fact>]
    let boolListEqTree () = 
        let list = List.map (fun x -> not x) (leafToList boolTree)
        let treeList = leafToList (map (fun x -> not x) boolTree)
        let expected =  [ false; false; true ]
        Assert.Equal<bool>(expected, list)
        Assert.Equal<bool>(expected, treeList)

    [<Fact>]
    let listListEqTree () = 
        let func list = List.map (fun x -> x * x) list
        let list = List.map func (leafToList listTree)
        let treeList = leafToList (map func listTree)
        let expected =  [ [1; 4; 9]; [25; 0; 0]; [4; 9] ]
        Assert.Equal<list<int>>(expected, list)
        Assert.Equal<list<int>>(expected, treeList)

    [<Fact>]
    let arrayListEqTree () = 
        let func list = Array.map (fun x -> x * x) list
        let list = List.map func (leafToList arrayTree)
        let treeList = leafToList (map func arrayTree)
        let expected =  [ [|1; 4; 9|]; [|25; 0; 0|]; [|4; 9|] ]
        Assert.Equal<array<int>>(expected, list)
        Assert.Equal<array<int>>(expected, treeList)


    [<Fact>]
    let intComposition () =
        let tree1 = map ((+) 2) (map ((+) 1) intTree)
        let tree2 = map ((+) 3) intTree
        let expected = Node [ Leaf 4; Node [ Leaf 5; Leaf 6 ]; Leaf 7 ]
        Assert.Equal<Tree<int>>(expected, tree1)
        Assert.Equal<Tree<int>>(expected, tree2)

    [<Fact>]
    let floatComposition () =
        let tree1 = leafToList (map ((+) 1.0) (map ((+) 2.1) floatTree))
        let tree2 = leafToList (map ((+) 3.1) floatTree)
        let expected = [ 9.87; 3.211; 4.433 ]
        Assert.True(List.forall2 (fun e a -> areAlmostEqual e a 1e-10) expected tree1)
        Assert.True(List.forall2 (fun e a -> areAlmostEqual e a 1e-10) expected tree2)

    [<Fact>]
    let charComposition () =
        let tree1 = map (fun c -> char (int c + 1)) (map (fun c -> char (int c + 1)) charTree)
        let tree2 = map (fun c -> char (int c + 2)) charTree
        let expected = Node [ Node [ Leaf 'c'; Leaf 't' ]; Leaf 'c' ]
        Assert.Equal<Tree<char>>(expected, tree1)
        Assert.Equal<Tree<char>>(expected, tree2)

    [<Fact>]
    let stringComposition () =
        let tree1 = map (fun s -> s + "a") (map (fun s -> s + "!") stringTree)
        let tree2 = map (fun s -> s + "!a") stringTree
        let expected = Node [ Node [ Leaf "Hello!a"; Leaf "World!a"; Leaf "!!a" ]; Leaf "f$harp!a" ]
        Assert.Equal<Tree<string>>(expected, tree1)
        Assert.Equal<Tree<string>>(expected, tree2)
        
    [<Fact>]
    let boolComposition () =
        let tree1 = map (fun x -> not x) (map (fun x -> not x) boolTree)
        let tree2 = map (fun x -> not (not x)) boolTree 
        let expected = Node [ Node [ Leaf true; Leaf true ]; Leaf false ]
        Assert.Equal<Tree<bool>>(expected, tree1)
        Assert.Equal<Tree<bool>>(expected, tree2)

    [<Fact>]
    let listComposition () =
        let f list a = List.map (fun x -> a * x) list 
        let tree1 = map (fun l -> f l 3) (map (fun l -> f l 2) listTree)
        let tree2 = map (fun l -> f l 6) listTree
        let expected = Node [ Node [ Leaf [6; 12; 18]; Leaf [30; 0; 0] ]; Leaf [12; 18] ]
        Assert.Equal<Tree<list<int>>>(expected, tree1)
        Assert.Equal<Tree<list<int>>>(expected, tree2)

    [<Fact>]
    let arrayComposition () =
        let f array a = Array.map (fun x -> a * x) array
        let tree1 = map (fun a -> f a 3) (map (fun a -> f a 2) arrayTree)
        let tree2 = map (fun a -> f a 6) arrayTree
        let expected = Node [ Node [ Leaf [|6; 12; 18|]; Leaf [|30; 0; 0|] ]; Leaf [|12; 18|] ]
        Assert.Equal<Tree<array<int>>>(expected, tree1)
        Assert.Equal<Tree<array<int>>>(expected, tree2)


    [<Fact>]
    let intHighIsConst () =
        let h = high (map ((+) 3) intTree) 
        Assert.Equal<int>(3, h)

    [<Fact>]
    let floatHighIsConst () =
        let h = high (map ((+) 3.0) floatTree)
        Assert.Equal<int>(3, h)

    [<Fact>]
    let charHighIsConst () =
        let h = high (map (fun c -> char (int c + 1)) charTree)
        Assert.Equal<int>(3, h)

    [<Fact>]
    let stringHighIsConst () =
        let h = high (map (fun s -> s + "!") stringTree)
        Assert.Equal<int>(3, h)

    [<Fact>]
    let boolHighIsConst () =
        let h = high (map (fun x -> x && true) boolTree)
        Assert.Equal<int>(3, h)

    [<Fact>]
    let listHighIsConst () =
        let f a list = List.map (fun x -> a * x) list
        let h = high (map (fun l -> f 2 l) listTree)
        Assert.Equal<int>(3, h)

    [<Fact>]
    let arrayHighIsConst () =
        let f a array = Array.map (fun x -> a * x) array
        let h = high (map (fun a -> f 2 a) arrayTree)
        Assert.Equal<int>(3, h)

    
module rFold_Eq_lFold =
    open dataAndFuncs

    [<Fact>]
    let intAdd () =
        let sumFunc acc value = acc + value
        let sum1 = leftFold sumFunc 0 intTree
        let sum2 = rightFold sumFunc 0 intTree
        Assert.Equal<int>(10, sum1)
        Assert.Equal<int>(10, sum2)

    [<Fact>]
    let floatAdd () =
        let sumFunc acc value = acc + value
        let sum1 = leftFold sumFunc 0.0 floatTree
        let sum2 = rightFold sumFunc 0.0 floatTree
        Assert.True(Math.Abs(sum1 - 8.214) < 1e-10)
        Assert.True(Math.Abs(sum2 - 8.214) < 1e-10)
        
    [<Fact>]
    let charAdd () =
        let sumFunc acc value = acc + (int value)
        let sum1 = leftFold sumFunc 0 charTree
        let sum2 = rightFold sumFunc 0 charTree
        Assert.Equal<int>(308, sum1)
        Assert.Equal<int>(308, sum2)

    [<Fact>]
    let stringAdd () =
        let sumFunc acc value = acc + value
        let sum1 = leftFold sumFunc "" stringTree
        let sum2 = rightFold sumFunc "" stringTree
        Assert.Equal<string>("HelloWorld!f$harp", sum1)
        Assert.Equal<string>("f$harp!WorldHello", sum2)

    [<Fact>]
    let boolAdd () =
        let orFunc acc value = acc || value
        let sum = rightFold orFunc true boolTree
        Assert.Equal<bool>(true, sum)


    [<Fact>]
    let intMult () =
        let sumFunc acc value = acc * value
        let sum1 = leftFold sumFunc 1 intTree
        let sum2 = rightFold sumFunc 1 intTree
        Assert.Equal<int>(24, sum1)
        Assert.Equal<int>(24, sum2)

    [<Fact>]
    let floatMult () =
        let sumFunc acc value = acc * value
        let sum1 = leftFold sumFunc 1.0 floatTree
        let sum2 = rightFold sumFunc 1.0 floatTree
        Assert.True(Math.Abs(sum1 - 1.00170951) < 1e-10)
        Assert.True(Math.Abs(sum2 - 1.00170951) < 1e-10)

    [<Fact>]
    let charMult () =
        let sumFunc acc value = acc * (int value)
        let sum1 = leftFold sumFunc 1 charTree
        let sum2 = rightFold sumFunc 1 charTree
        Assert.Equal<int>(1072626, sum1)
        Assert.Equal<int>(1072626, sum2)

    [<Fact>]
    let boolMult () =
        let andFunc acc value = acc && value
        let sum = leftFold andFunc false boolTree
        sum = false |> ignore
        Assert.Equal<bool>(false, sum)

   
module tHighOfTree =
    open dataAndFuncs

    [<Fact>]
    let high () =
        let intHigh = high intTree
        let floatHigh = high floatTree
        let charHigh = high charTree
        let stringHigh =high stringTree
        let boolHigh = high boolTree
        let listHigh = high listTree
        let arrayHigh = high arrayTree

        Assert.Equal(3, intHigh)
        Assert.Equal(3, floatHigh)
        Assert.Equal(3, charHigh)
        Assert.Equal(3, stringHigh)
        Assert.Equal(3, boolHigh)
        Assert.Equal(3, listHigh)
        Assert.Equal(3, arrayHigh)
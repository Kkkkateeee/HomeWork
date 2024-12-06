namespace PropertyTrees

open System
open Xunit
open FsCheck
open FsCheck.Xunit

open Trees
open Trees.Tree
open UnitTrees.dataAndFuncs


[<Properties(MaxTest = 100)>]
type mapProp() =

    [<Property>]
    member _.intId (tree: Tree<int>) =
        tree = map id tree

    [<Property>]
    member _.floatId (tree: Tree<float32>) =
        let list = leafToList tree
        let idList = leafToList (map id tree)
        Assert.True(List.forall2 (fun a b -> areAlmostEqual a b 1e-10f)list idList)

    [<Property>]
    member _.charId (tree: Tree<char>) =
        tree = map id tree

    [<Property>]
    member _.arrayId (tree: Tree<array<int>>) =
        tree = map id tree


    [<Property>]
    member _.intListEqTree (tree: Tree<int>) = 
        let list = List.map ((+) 1 ) (leafToList tree)
        let treeList = leafToList (map (fun x -> x + 1) tree)
        list = treeList 

    [<Property>]
    member _.floatListEqTree (tree: Tree<float32>) = 
        let list = List.map ((+) 1.0f) (leafToList tree)
        let treeList = leafToList (map (fun x -> x + 1.0f) tree)
        Assert.True(List.forall2 (fun a b -> areAlmostEqual a b 1e-10f)list treeList)

    [<Property>]
    member _.charListEqTree (tree: Tree<char>) = 
        let list = List.map (fun c -> char (int c + 1)) (leafToList tree)
        let treeList = leafToList (map (fun c -> char (int c + 1)) tree)
        list = treeList

    [<Property>]
    member _.arrayListEqTree (tree: Tree<array<int>>) = 
        let func list = Array.map (fun x -> x * x) list
        let list = List.map func (leafToList tree)
        let treeList = leafToList (map func tree)
        list = treeList


    [<Property>]
    member _.intComposition (tree: Tree<int>) =
        let tree1 = map ((+) 2) (map ((+) 1) tree)
        let tree2 = map ((+) 3) tree
        tree1 = tree2

    [<Property>]
    member _.floatComposition (tree: Tree<float32>)  =
        let tree1 = leafToList (map ((+) 2.0f) (map ((+) 1.0f) tree))
        let tree2 = leafToList (map ((+) 3.0f) tree )
        Assert.True(List.forall2 (fun a b -> areAlmostEqual a b 1e-5f) tree1 tree2)

    [<Property>]
    member _.charComposition (tree: Tree<char>)  =
        let tree1 = map (fun c -> char (int c + 1)) (map (fun c -> char (int c + 1)) tree)
        let tree2 = map (fun c -> char (int c + 2)) tree
        tree1 = tree2

    [<Property>]
    member _.arrayComposition (tree: Tree<array<int>>) =
        let f array a = Array.map (fun x -> a * x) array
        let tree1 = map (fun a -> f a 3) (map (fun a -> f a 2) tree)
        let tree2 = map (fun a -> f a 6) tree
        tree1 = tree2


    [<Property>]
    member _.intHighIsConst (tree: Tree<int>) =
        let h1 = high (map ((+) 3) tree) 
        let h2 = high (map  ((+) 5) tree) 
        h1 = h2

    [<Property>]
    member _.floatHighIsConst (tree: Tree<float32>) =
        let h1 = high (map ((+) 3.0f) tree)
        let h2 = high (map ((+) 5.6f) tree)
        h1 = h2

    [<Property>]
    member _.charHighIsConst (tree: Tree<char>) =
        let h1 = high (map (fun c -> char (int c + 1)) tree)
        let h2 = high (map (fun c -> char (int c + 2)) tree)
        h1 = h2

    [<Property>]
    member _.arrayHighIsConst (tree: Tree<array<int>>) =
        let f a array = Array.map (fun x -> a * x) array
        let h1 = high (map (fun a -> f 2 a) tree)
        let h2 = high (map (fun a -> f 3 a) tree)
        h1 = h2


[<Properties(MaxTest = 100)>]
type leftRightFold() =

    [<Property>]
    member _.intAdd (tree: Tree<int>) =
        let sumFunc acc value = acc + value
        let sum1 = foldLeft sumFunc 0 tree
        let sum2 = foldRight sumFunc 0 tree
        sum1 = sum2

    [<Property>]
    member _.floatAdd (tree: Tree<float32>) =
        let sumFunc acc value = acc + value
        let sum1 = foldLeft sumFunc 0.0f tree
        let sum2 = foldRight sumFunc 0.0f tree
        Assert.Equal(sum1, sum2, 1)

    [<Property>]
    member _.charAdd (tree: Tree<char>) =
        let sumFunc acc value = acc + (int value + 1)
        let sum1 = foldLeft sumFunc 0 tree
        let sum2 = foldRight sumFunc 0 tree
        sum1 = sum2


    [<Property>]
    member _.intMult (tree: Tree<int>) =
        let sumFunc acc value = acc * value
        let sum1 = foldLeft sumFunc 1 tree
        let sum2 = foldRight sumFunc 1 tree
        sum1 = sum2

    [<Property>]
    member _.floatMult (tree: Tree<float32>) =
        let sumFunc acc value = acc * value
        let sum1 = foldLeft sumFunc 0.0f tree
        let sum2 = foldRight sumFunc 0.0f tree
        Assert.Equal(sum1, sum2, 1)

    [<Property>]
    member _.charMult (tree: Tree<char>) =
        let sumFunc acc value = acc * (int value + 1)
        let sum1 = foldLeft sumFunc 0 tree
        let sum2 = foldRight sumFunc 0 tree
        sum1 = sum2
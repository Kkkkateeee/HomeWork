namespace PropertyTrees

open System
open Xunit
open FsCheck
open FsCheck.Xunit

open Trees
open Trees.Tree
open UnetTrees.dataAndFuncs

type Overrides() =  // float generator so that zeros and infinities wouldn't get into the trees
    static member Float() =
        Arb.Default.Float()
        |> Arb.filter (fun f -> not <| Double.IsNaN(f) && not <| Double.IsInfinity(f))

        
[<Properties(MaxTest = 10)>]
type mapProp() =

    [<Property>]
    member _.intId (tree: Tree<int>) =
        tree = map id tree

    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _.floatId (tree: Tree<float>) =
        tree = map id tree

    [<Property>]
    member _.charId (tree: Tree<char>) =
        tree = map id tree

    [<Property>]
    member _.stringId (tree: Tree<string>) =
        tree = map id tree

    [<Property>]
    member _.boolId (tree: Tree<bool>) =
        tree = map id tree

    [<Property>]
    member _.listId (tree: Tree<list<int>>) =
        tree = map id tree

    [<Property>]
    member _.arrayId (tree: Tree<array<int>>) =
        tree = map id tree


    [<Property>]
    member _. intListEqTree (tree: Tree<int>) = 
        let list = List.map ((+) 1 ) (leafToList tree)
        let treeList = leafToList (map (fun x -> x + 1) tree)
        list = treeList 

    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _. floatListEqTree (tree: Tree<float>) = 
        let list = List.map ((+) 1.0 ) (leafToList tree)
        let treelist = leafToList (map (fun x -> x + 1.0) tree)
        list = treelist

    [<Property>]
    member _. chatListEqTree (tree: Tree<char>) = 
        let list = List.map (fun c -> char (int c + 1)) (leafToList tree)
        let treelist = leafToList (map (fun c -> char (int c + 1)) tree)
        list = treelist

    [<Property>]
    member _. stringListEqTree (tree: Tree<string>) = 
        let list = List.map (fun s -> s + "!") (leafToList tree)
        let treeList = leafToList (map (fun s -> s + "!") tree)
        list = treeList 

    [<Property>]
    member _. boolListEqTree (tree: Tree<bool>) = 
        let list = List.map (fun x -> not x) (leafToList tree)
        let treelist = leafToList (map (fun x -> not x) tree)
        list = treelist

    [<Property>]
    member _. listListEqTree (tree: Tree<list<int>>) = 
        let func list = List.map (fun x -> x * x) list
        let list = List.map func (leafToList tree)
        let treelist = leafToList (map func tree)
        list = treelist

    [<Property>]
    member _.  arrayListEqTree (tree: Tree<array<int>>) = 
        let func list = Array.map (fun x -> x * x) list
        let list = List.map func (leafToList tree)
        let treelist = leafToList (map func tree)
        list = treelist


    [<Property>]
    member _. intComposition (tree: Tree<int>) =
        let tree1 = map ((+) 2) (map ((+) 1) tree)
        let tree2 = map ((+) 3) tree
        tree1 = tree2

    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _. floatComposition (tree: Tree<float>)  =
        let tree1 = leafToList (map ((+) 2.1) (map ((+) 1.0) tree))
        let tree2 = leafToList (map ((+) 3.1) tree )
        Assert.True(List.forall2 (fun e a -> areAlmostEqual e a 1e-10) tree1 tree2)

    [<Property>]
    member _. charComposition (tree: Tree<char>)  =
        let tree1 = map (fun c -> char (int c + 1)) (map (fun c -> char (int c + 1)) tree)
        let tree2 = map (fun c -> char (int c + 2)) tree
        tree1 = tree2

    [<Property>]
    member _. stringComposition (tree: Tree<string>) =
        let tree1 = map (fun s -> s + "a") (map (fun s -> s + "!") tree)
        let tree2 = map (fun s -> s + "!a") tree
        tree1 = tree2

    [<Property>]
    member _. boolComposition (tree: Tree<bool>) =
        let tree1 = map (fun x -> not x) (map (fun x -> not x) tree)
        let tree2 = map (fun x -> not (not x)) tree 
        tree1 = tree2

    [<Property>]
    member _. listComposition (tree: Tree<list<int>>) =
        let f list a = List.map (fun x -> a * x) list
        let tree1 = map (fun l -> f l 3) (map (fun l -> f l 2) tree)
        let tree2 = map (fun l -> f l 6) tree
        tree1 = tree2

    [<Property>]
    member _. arrayComposition (tree: Tree<array<int>>) =
        let f array a = Array.map (fun x -> a * x) array
        let tree1 = map (fun a -> f a 3) (map (fun a -> f a 2) tree)
        let tree2 = map (fun a -> f a 6) tree
        tree1 = tree2


    [<Property>]
    member _. intHighIsConst (tree: Tree<int>) =
        let h1 = high (map ((+) 3) tree) 
        let h2 = high (map  ((+) 5) tree) 
        h1 = h2

    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _. floatHighIsConst (tree: Tree<float>) =
        let h1 = high (map ((+) 3.0) tree)
        let h2 = high (map ((+) 5.6) tree)
        h1 = h2

    [<Property>]
    member _. charHighIsConst (tree: Tree<char>) =
        let h1 = high (map (fun c -> char (int c + 1)) tree)
        let h2 = high (map (fun c -> char (int c + 2)) tree)
        h1 = h2

    [<Property>]
    member _. stringHighIsConst (tree: Tree<string>) =
        let h1 = high (map (fun s -> s + "!") tree)
        let h2 = high (map (fun s -> s + "!!")tree)
        h1 = h2

    [<Property>]
    member _. boolHighIsConst (tree: Tree<bool>) =
        let h1 = high (map (fun x -> x && true) tree)
        let h2 = high (map (fun x -> x || false) tree)
        h1 = h2

    [<Property>]
    member _. listHighIsConst (tree: Tree<list<int>>) =
        let f a list = List.map (fun x -> a * x) list
        let h1 = high (map (fun l -> f 2 l) tree)
        let h2 = high (map (fun l -> f 3 l) tree)
        h1 = h2

    [<Property>]
    member _. arrayHighIsConst (tree: Tree<array<int>>) =
        let f a array = Array.map (fun x -> a * x) array
        let h1 = high (map (fun a -> f 2 a) tree)
        let h2 = high (map (fun a -> f 3 a) tree)
        h1 = h2


[<Properties(MaxTest = 10)>]
type leftRightFold() =

    [<Property>]
    member _. intAdd (tree: Tree<int>) =
        let sumFunc acc value = acc + value
        let sum1 = leftFold sumFunc 0 tree
        let sum2 = rightFold sumFunc 0 tree
        sum1 = sum2

    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _.floatAdd (tree: Tree<float>) =
        let sumFunc (acc: float) (value: float) =  // controls the overflow of the float value
            let absValue = Math.Abs(float value) 
            if Math.Abs(acc) < Double.MaxValue - absValue then
                acc + value
            else
                Double.MaxValue

        let sum1 = leftFold sumFunc 0.0 tree
        let sum2 = rightFold sumFunc 0.0 tree
        Assert.True(Math.Abs(sum1 - sum2) < 1e-10)

    [<Property>]
    member _. charAdd (tree: Tree<char>) =
        let sumFunc acc value = acc + (int value + 1)
        let sum1 = leftFold sumFunc 0 tree
        let sum2 = rightFold sumFunc 0 tree
        sum1 = sum2

    [<Property>]
    member _. boolAdd (tree: Tree<bool>) =
        let orFunc  acc value = acc || value
        let sum = rightFold orFunc true tree
        sum = true


    [<Property>]
    member _. intMult (tree: Tree<int>) =
        let sumFunc acc value = acc * value
        let sum1 = leftFold sumFunc 1 tree
        let sum2 = rightFold sumFunc 1 tree
        sum1 = sum2

    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _. floatMult (tree: Tree<float>) =
        let sumFunc (acc: float) (value: float) =  // controls the overflow of the float value
            let absValue = Math.Abs(float value) 
            if Math.Abs(acc) < Double.MaxValue - absValue then
                acc * value
            else
                Double.MaxValue

        let sum1 = leftFold sumFunc 0.0 tree
        let sum2 = rightFold sumFunc 0.0 tree
        Assert.True(Math.Abs(sum1 - sum2) < 1e-10)

    [<Property>]
    member _. charMult (tree: Tree<char>) =
        let sumFunc acc value = acc * (int value + 1)
        let sum1 = leftFold sumFunc 0 tree
        let sum2 = rightFold sumFunc 0 tree
        sum1 = sum2

    [<Property>]
    member _. boolMult (tree: Tree<bool>) =
        let andFunc acc value = acc && value
        let sum = leftFold andFunc false tree
        sum = false |> ignore
module RandomTests

open Xunit
open FsCheck
open FsCheck.Xunit
open Library.MyList
open FsCheck.Arb

type Overrides() =
    static member Float() =
        Arb.Default.Float()
        |> filter (fun f -> not <| System.Double.IsNaN(f) && not <| System.Double.IsInfinity(f))

Arb.register<Overrides> () |> ignore


[<Properties(MaxTest = 10)>]
type RandomTestsForBubleSort() =

    [<Property>]
    member _.IntTest(test_cases: int list) =
        let actual = bubble_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)

    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _.FloatTest(test_cases: float list) =
        let actual = bubble_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)

    [<Property>]
    member _.CharTest(test_cases: char list) =
        let actual = bubble_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)


[<Properties(MaxTest = 10)>]
type RandomTestsForQuickSor1t() =

    [<Property>]
    member _.IntTest(test_cases: int list) =
        let actual = quick_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)

    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _.FloatTest(test_cases: float list) =
        let actual = quick_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)

    [<Property>]
    member _.CharTest(test_cases: char list) =
        let actual = quick_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)


[<Properties(MaxTest = 10)>]
type RandomTestsForMergeSort() =

    [<Property>]
    member _.IntTest(test_cases: int list) =
        let actual = merge_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)

    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _.FloatTest(test_cases: float list) =
        let actual = merge_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)

    [<Property>]
    member _.CharTest(test_cases: char list) =
        let actual = merge_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)


[<Properties(MaxTest = 10)>]
type BubbleEqualsQuick() =

    [<Property>]
    member _.IntTest(test_cases: int list) =
        let bubble = bubble_sort (List_MyList test_cases) |> MyList_List
        let quick = quick_sort (List_MyList test_cases) |> MyList_List
        Assert.True(bubble.Length = quick.Length && List.forall2 (=) bubble quick)

    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _.FloatTest(test_cases: float list) =
        let bubble = bubble_sort (List_MyList test_cases) |> MyList_List
        let quick = quick_sort (List_MyList test_cases) |> MyList_List
        Assert.True(bubble.Length = quick.Length && List.forall2 (=) bubble quick)

    [<Property>]
    member _.CharTest(test_cases: char list) =
        let bubble = bubble_sort (List_MyList test_cases) |> MyList_List
        let quick = quick_sort (List_MyList test_cases) |> MyList_List
        Assert.True(bubble.Length = quick.Length && List.forall2 (=) bubble quick)


[<Properties(MaxTest = 10)>]
type BubbleEqualsMerge() =

    [<Property>]
    member _.IntTest(test_cases: int list) =
        let bubble = bubble_sort (List_MyList test_cases) |> MyList_List
        let merge = merge_sort (List_MyList test_cases) |> MyList_List
        Assert.True(bubble.Length = merge.Length && List.forall2 (=) bubble merge)

    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _.FloatTest(test_cases: float list) =
        let bubble = bubble_sort (List_MyList test_cases) |> MyList_List
        let merge = merge_sort (List_MyList test_cases) |> MyList_List
        Assert.True(bubble.Length = merge.Length && List.forall2 (=) bubble merge)

    [<Property>]
    member _.CharTest(test_cases: char list) =
        let bubble = bubble_sort (List_MyList test_cases) |> MyList_List
        let merge = merge_sort (List_MyList test_cases) |> MyList_List
        Assert.True(bubble.Length = merge.Length && List.forall2 (=) bubble merge)


[<Properties(MaxTest = 10)>]
type MergeEqualsQuick() =

    [<Property>]
    member _.IntTest(test_cases: int list) =
        let merge = bubble_sort (List_MyList test_cases) |> MyList_List
        let quick = quick_sort (List_MyList test_cases) |> MyList_List
        Assert.True(merge.Length = quick.Length && List.forall2 (=) merge quick)

    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _.FloatTest(test_cases: float list) =
        let merge = bubble_sort (List_MyList test_cases) |> MyList_List
        let quick = quick_sort (List_MyList test_cases) |> MyList_List
        Assert.True(merge.Length = quick.Length && List.forall2 (=) merge quick)

    [<Property>]
    member _.CharTest(test_cases: char list) =
        let merge = bubble_sort (List_MyList test_cases) |> MyList_List
        let quick = quick_sort (List_MyList test_cases) |> MyList_List
        Assert.True(merge.Length = quick.Length && List.forall2 (=) merge quick)

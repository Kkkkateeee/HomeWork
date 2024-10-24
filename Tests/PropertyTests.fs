module PropertyTests

open Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.Arb

open MyListSorts.SortFunctions
open MyListSorts

type Overrides() =
    static member Float() =
        Arb.Default.Float()
        |> filter (fun f -> not <| System.Double.IsNaN(f) && not <| System.Double.IsInfinity(f))

Arb.register<Overrides> () |> ignore


[<Properties(MaxTest = 10)>]
type PropertyTestsForBubleSort() =

    [<Property>]
    member _.intTest(testCases: int list) =
        let actual = bubbleSort (MyList.toMyList testCases) |> MyList.toList
        let expected = List.sort testCases
        Assert.Equal<int>(expected, actual)

    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _.floatTest(testCases: float list) =
        let actual = bubbleSort (MyList.toMyList testCases) |> MyList.toList
        let expected = List.sort testCases
        Assert.Equal<float>(expected, actual)

    [<Property>]
    member _.charTest(testCases: char list) =
        let actual = bubbleSort (MyList.toMyList testCases) |> MyList.toList
        let expected = List.sort testCases
        Assert.Equal<char>(expected, actual)

[<Properties(MaxTest = 10)>]
type PropertyTestsForQuickSor1t() =

    [<Property>]
    member _.intTest(testCases: int list) =
        let actual = quickSort (MyList.toMyList testCases) |> MyList.toList
        let expected = List.sort testCases
        Assert.Equal<int>(expected, actual)

    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _.floatTest(testCases: float list) =
        let actual = quickSort (MyList.toMyList testCases) |> MyList.toList
        let expected = List.sort testCases
        Assert.Equal<float>(expected, actual)

    [<Property>]
    member _.fharTest(testCases: char list) =
        let actual = quickSort (MyList.toMyList testCases) |> MyList.toList
        let expected = List.sort testCases
        Assert.Equal<char>(expected, actual)

[<Properties(MaxTest = 10)>]
type RandomTestsForMergeSort() =

    [<Property>]
    member _.intTest(testCases: int list) =
        let actual = mergeSort (MyList.toMyList testCases) |> MyList.toList
        let expected = List.sort testCases
        Assert.Equal<int>(expected, actual)

    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _.floatTest(testCases: float list) =
        let actual = mergeSort (MyList.toMyList testCases) |> MyList.toList
        let expected = List.sort testCases
        Assert.Equal<float>(expected, actual)

    [<Property>]
    member _.charTest(testCases: char list) =
        let actual = mergeSort (MyList.toMyList testCases) |> MyList.toList
        let expected = List.sort testCases
        Assert.Equal<char>(expected, actual)




[<Properties(MaxTest = 10)>]
type BubbleEqualsQuick() =

    [<Property>]
    member _.intTest(testCases: int list) =
        let bubble = bubbleSort (MyList.toMyList testCases) |> MyList.toList
        let quick = quickSort (MyList.toMyList testCases) |> MyList.toList
        Assert.Equal<int>(bubble, quick)

    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _.floatTest(testCases: float list) =
        let bubble = bubbleSort (MyList.toMyList testCases) |> MyList.toList
        let quick = quickSort (MyList.toMyList testCases) |> MyList.toList
        Assert.Equal<float>(bubble, quick)

    [<Property>]
    member _.charTest(testCases: char list) =
        let bubble = bubbleSort (MyList.toMyList testCases) |> MyList.toList
        let quick = quickSort (MyList.toMyList testCases) |> MyList.toList
        Assert.Equal<char>(bubble, quick)

[<Properties(MaxTest = 10)>]
type BubbleEqualsMerge() =

    [<Property>]
    member _.ifntTest(testCases: int list) =
        let bubble = bubbleSort (MyList.toMyList testCases) |> MyList.toList
        let merge = mergeSort (MyList.toMyList testCases) |> MyList.toList
        Assert.Equal<int>(bubble, merge)

    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _.floatTest(testCases: float list) =
        let bubble = bubbleSort (MyList.toMyList testCases) |> MyList.toList
        let merge = mergeSort (MyList.toMyList testCases) |> MyList.toList
        Assert.Equal<float>(bubble, merge)

    [<Property>]
    member _.charTest(testCases: char list) =
        let bubble = bubbleSort (MyList.toMyList testCases) |> MyList.toList
        let merge = mergeSort (MyList.toMyList testCases) |> MyList.toList
        Assert.Equal<char>(bubble, merge)


[<Properties(MaxTest = 10)>]
type MergeEqualsQuick() =

    [<Property>]
    member _.intTest(testCases: int list) =
        let merge = bubbleSort (MyList.toMyList testCases) |> MyList.toList
        let quick = quickSort (MyList.toMyList testCases) |> MyList.toList
        Assert.Equal<int>(merge, quick)
    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _.floatTest(testCases: float list) =
        let merge = bubbleSort (MyList.toMyList testCases) |> MyList.toList
        let quick = quickSort (MyList.toMyList testCases) |> MyList.toList
        Assert.Equal<float>(merge, quick)

    [<Property>]
    member _.charTest(testCases: char list) =
        let merge = bubbleSort (MyList.toMyList testCases) |> MyList.toList
        let quick = quickSort (MyList.toMyList testCases) |> MyList.toList
        Assert.Equal<char>(merge, quick)
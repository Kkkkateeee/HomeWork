module RandomTests

open Xunit
open FsCheck
open FsCheck.Xunit
open Library.MyList
open FsCheck.Arb

type Overrides() =
    static member Float() =
        Arb.Default.Float()
        |> filter (fun f -> not <| System.Double.IsNaN(f) &&
                            not <| System.Double.IsInfinity(f)) 
Arb.register<Overrides>() |> ignore


[<Properties(MaxTest=10)>]
type RandomTestsForBubleSort () =

    [<Property>]
    member _.IntTest  (test_cases: int list) =
        let actual = bubble_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)
    
    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _.FloatTest(test_cases: float list) =
        let actual = bubble_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)

    [<Property>]
    member _.CharTest  (test_cases: char list) =
        let actual = bubble_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)


type RandomTestsForQuickSor1t () = 

    [<Property>]
    member _.IntTest  (test_cases: int list) =
        let actual =  quick_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)
        
    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _.FloatTest  (test_cases: float list) =
        let actual = quick_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)

    [<Property>]
    member _.CharTest  (test_cases: char list) =
        let actual = quick_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)


type RandomTestsForMergeSort () =

    [<Property>]
    member _.IntTest  (test_cases: int list) =
        let actual =  merge_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)
        
    [<Property(Arbitrary = [| typeof<Overrides> |])>]
    member _.FloatTest  (test_cases: float list) =
        let actual = merge_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)

    [<Property>]
    member _.CharTest  (test_cases: char list) =
        let actual = merge_sort (List_MyList test_cases) |> MyList_List
        let expected = List.sort test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)

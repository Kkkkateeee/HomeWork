module ModuleTests

open Xunit

open MyListSorts
open MyListSorts.TypeConvertFunctions
open MyListSorts.SortFunctions


module module_test =

    let intTest (f: MyList<int> -> MyList<int>) =
        let int_test_cases =
            [ []
              [ 1; 2; 3; 4; 5 ]
              [ 5; 4; 3; 2; 1 ]
              [ 3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5 ]
              [ 4; 2; 2; 8; 3; 3; 1 ] ]

        for elem in int_test_cases do
            let actual = f (ConvertListToMyList elem) |> ConvertMyListToList
            let expected = List.sort elem
            Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)


    let floatTest (f: MyList<float> -> MyList<float>) =
        let float_test_cases = [ 3.14; 1.41; 2.71; 0.57; 4.67 ]
        let actual = f (ConvertListToMyList float_test_cases) |> ConvertMyListToList
        let expected = List.sort float_test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)


    let charTest (f: MyList<char> -> MyList<char>) =
        let char_test_cases = [ 'D'; 'A'; 'C'; 'B'; 'E' ]
        let actual = f (ConvertListToMyList char_test_cases) |> ConvertMyListToList
        let expected = List.sort char_test_cases
        Assert.True(expected.Length = actual.Length && List.forall2 (=) expected actual)


    [<Fact>]
    let ``intTestBubbleSort`` () = intTest bubble_sort

    [<Fact>]
    let ``floatTestBubbleSort`` () = floatTest bubble_sort

    [<Fact>]
    let ``charTestBubbleSort`` () = charTest bubble_sort


    [<Fact>]
    let ``intTestQuickSort`` () = intTest quick_sort

    [<Fact>]
    let ``floatTestQuickSort`` () = floatTest quick_sort

    [<Fact>]
    let ``charTestQuickSort`` () = charTest quick_sort


    [<Fact>]
    let ``intTestMergeSort`` () = intTest merge_sort

    [<Fact>]
    let ``floatTestMergeSort`` () = floatTest merge_sort

    [<Fact>]
    let ``charTestMergeSort`` () = charTest merge_sort

module ModuleTests

open Xunit
open Library.sorts


module type_changer =
    
    let rec List_MyList (list: 't List) : (MyList<'t>) =
        match list with 
        | [] -> Empty
        | head :: tail -> Cons(head, List_MyList tail)

    let rec MyList_List (list: MyList<'t>) : ('t List) =
        match list with 
        | Empty -> []
        | Cons(head, tail) -> head :: MyList_List tail


module module_test =

    let intTest (f: MyList<int> -> MyList<int>) =
        let int_test_cases =
            [  []
               [ 1; 2; 3; 4; 5 ]
               [ 5; 4; 3; 2; 1 ]
               [ 3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5 ]
               [ 4; 2; 2; 8; 3; 3; 1 ] ]

        for elem in int_test_cases do
            let actual = f (type_changer.List_MyList elem) |> type_changer.MyList_List
            let expected = List.sort elem
            Assert.Equal(expected.Length, actual.Length)

            for i in 0 .. expected.Length - 1 do
                Assert.Equal(expected.[i], actual.[i])


    let floatTest (f: MyList<float> -> MyList<float>) =
        let float_test_cases = [ 3.14; 1.41; 2.71; 0.57; 4.67 ]
        let actual = f (type_changer.List_MyList float_test_cases) |> type_changer.MyList_List
        let expected = List.sort float_test_cases
        Assert.Equal(expected.Length, actual.Length)

        for i in 0 .. expected.Length - 1 do
            Assert.Equal(expected.[i], actual.[i])


    let charTest (f: MyList<char> -> MyList<char>) =
        let char_test_cases = [ 'D'; 'A'; 'C'; 'B'; 'E' ]
        let actual = f (type_changer.List_MyList char_test_cases) |> type_changer.MyList_List
        let expected = List.sort char_test_cases
        Assert.Equal(expected.Length, actual.Length)

        for i in 0 .. expected.Length - 1 do
            Assert.Equal(expected.[i], actual.[i])


    [<Fact>]
    let  ``intTestBubbleSort``() = intTest bubble_sort

    [<Fact>]
    let  ``floatTestBubbleSort``() = floatTest bubble_sort

    [<Fact>]
    let  ``charTestBubbleSort``() = charTest bubble_sort


    [<Fact>]
    let  ``intTestQuickSort``() = intTest quick_sort

    [<Fact>]
    let  ``floatTestQuickSort``() = floatTest quick_sort

    [<Fact>]
    let  ``charTestQuickSort``() = charTest quick_sort


    [<Fact>]
    let  ``intTestMergeSort``() = intTest merge_sort

    [<Fact>]
    let  ``floatTestMergeSort``() = floatTest merge_sort

    [<Fact>]
    let  ``charTestMergeSort``() = charTest merge_sort
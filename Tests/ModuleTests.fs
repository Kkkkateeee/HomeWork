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

    let int_test_cases =
        [   []
            [1]
            [ 1; 2; 3; 4; 5 ]
            [ 5; 4; 3; 2; 1 ]
            [ 3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5 ]
            [ 4; 2; 2; 8; 3; 3; 1 ] ]

    let float_test_cases = [ 3.14; 1.41; 2.71; 0.57; 4.67 ]
    let char_test_cases = [ 'D'; 'A'; 'C'; 'B'; 'E' ]

    [<Fact>]
    let ``TestBubbleSort_int`` () = 
        for elem in int_test_cases do
            let res = bubble_sort (type_changer.List_MyList elem)
            let expected = List.sort elem
            let actual = type_changer.MyList_List res
            Assert.Equal<int list>(expected, actual)

    [<Fact>]
    let ``TestBubbleSort_float`` () = 
        let res = bubble_sort (type_changer.List_MyList float_test_cases)
        let expected = List.sort float_test_cases
        let actual = type_changer.MyList_List res
        Assert.Equal<float list>(expected, actual)

    [<Fact>]
    let ``TestBubbleSort_char`` () = 
        let res = bubble_sort (type_changer.List_MyList char_test_cases)
        let expected = List.sort char_test_cases
        let actual = type_changer.MyList_List res
        Assert.Equal<char list>(expected, actual)


    [<Fact>]
    let ``TestQuickSort_int`` () = 
        for elem in int_test_cases do
            let res = bubble_sort (type_changer.List_MyList elem)
            let expected = List.sort elem
            let actual = type_changer.MyList_List res
            Assert.Equal<int list>(expected, actual)

    [<Fact>]
    let ``TestQuickSort_float`` () = 
        let res = bubble_sort (type_changer.List_MyList float_test_cases)
        let expected = List.sort float_test_cases
        let actual = type_changer.MyList_List res
        Assert.Equal<float list>(expected, actual)

    [<Fact>]
    let ``TestQuickSort_char`` () = 
        let res = bubble_sort (type_changer.List_MyList char_test_cases)
        let expected = List.sort char_test_cases
        let actual = type_changer.MyList_List res
        Assert.Equal<char list>(expected, actual)


    [<Fact>]
    let ``TestMergeSort_int`` () = 
        for elem in int_test_cases do
            let res = bubble_sort (type_changer.List_MyList elem)
            let expected = List.sort elem
            let actual = type_changer.MyList_List res
            Assert.Equal<int list>(expected, actual)

    [<Fact>]
    let ``TestMergeSort_float`` () = 
        let res = bubble_sort (type_changer.List_MyList float_test_cases)
        let expected = List.sort float_test_cases
        let actual = type_changer.MyList_List res
        Assert.Equal<float list>(expected, actual)

    [<Fact>]
    let ``TestMergeSort_char`` () = 
        let res = bubble_sort (type_changer.List_MyList char_test_cases)
        let expected = List.sort char_test_cases
        let actual = type_changer.MyList_List res
        Assert.Equal<char list>(expected, actual)
module UnitTests

open Xunit

open MyListSorts
open MyListSorts.SortFunctions


module UnitTests =

    let intTest (f: MyList<int> -> MyList<int>) =
        let intTestCases =
            [ []
              [ 1; 2; 3; 4; 5 ]
              [ 5; 4; 3; 2; 1 ]
              [ 3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5 ]
              [ 4; 2; 2; 8; 3; 3; 1 ] ]

        for elem in intTestCases do
            let actual = f (MyList.toMyList elem) |> MyList.toList
            let expected = List.sort elem
            Assert.Equal<int>(expected, actual)


    let floatTest (f: MyList<float> -> MyList<float>) =
        let floatTestCases = [ 3.14; 1.41; 2.71; 0.57; 4.67 ]
        let actual = f (MyList.toMyList floatTestCases) |> MyList.toList
        let expected = List.sort floatTestCases
        Assert.Equal<float>(expected, actual)

    let charTest (f: MyList<char> -> MyList<char>) =
        let charTestCases = [ 'D'; 'A'; 'C'; 'B'; 'E' ]
        let actual = f (MyList.toMyList charTestCases) |> MyList.toList
        let expected = List.sort charTestCases
        Assert.Equal<char>(expected, actual)

    [<Fact>]
    let intTestBubbleSort () = intTest bubbleSort

    [<Fact>]
    let floatTestBubbleSort () = floatTest bubbleSort

    [<Fact>]
    let charTestBubbleSort () = charTest bubbleSort


    [<Fact>]
    let intTestQuickSort () = intTest quickSort

    [<Fact>]
    let floatTestQuickSort () = floatTest quickSort

    [<Fact>]
    let charTestQuickSort () = charTest quickSort


    [<Fact>]
    let intTestMergeSort () = intTest mergeSort

    [<Fact>]
    let floatTestMergeSort () = floatTest mergeSort

    [<Fact>]
    let charTestMergeSort () = charTest mergeSort

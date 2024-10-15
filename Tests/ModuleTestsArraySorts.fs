namespace ModuleTestsArraySorts

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open ArraySorts.sorts


[<TestClass>]

type sorts_test() =
    let intTest (f: array<int> -> array<int>) =
        let int_test_cases =
            [| [||]
               [| 1; 2; 3; 4; 5 |]
               [| 5; 4; 3; 2; 1 |]
               [| 3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5 |]
               [| 4; 2; 2; 8; 3; 3; 1 |] |]

        for elem in int_test_cases do
            let actual = f elem
            let expected = Array.sort elem
            Assert.IsTrue(expected.Length = actual.Length && Array.forall2 (=) expected actual)

    let floatTest (f: array<float> -> array<float>) =
        let float_test_cases = [| 3.14; 1.41; 2.71; 0.57; 4.67 |]
        let actual = f float_test_cases
        let expected = Array.sort float_test_cases
        Assert.IsTrue(expected.Length = actual.Length && Array.forall2 (=) expected actual)

    let charTest (f: array<char> -> array<char>) =
        let char_test_cases = [| 'D'; 'A'; 'C'; 'B'; 'E' |]
        let actual = f char_test_cases
        let expected = Array.sort char_test_cases
        Assert.IsTrue(expected.Length = actual.Length && Array.forall2 (=) expected actual)


    [<TestMethod>]
    member this.intTestBubbleSort() = intTest bubble_sort

    [<TestMethod>]
    member this.floatTestBubbleSort() = floatTest bubble_sort

    [<TestMethod>]
    member this.charTestBubbleSort() = charTest bubble_sort


    [<TestMethod>]
    member this.intTestQuickSort() = intTest bubble_sort

    [<TestMethod>]
    member this.floatTestQuickSort() = floatTest bubble_sort

    [<TestMethod>]
    member this.charTestQuickSort() = charTest bubble_sort


    [<TestMethod>]
    member this.intTestMergeSort() = intTest bubble_sort

    [<TestMethod>]
    member this.floatTestMergeSort() = floatTest bubble_sort

    [<TestMethod>]
    member this.charTestMergeSort() = charTest bubble_sort

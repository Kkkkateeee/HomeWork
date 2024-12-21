namespace UnitTestsArraySorts

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open ArraySorts.Arrays


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
            Assert.AreEqual(expected.Length, actual.Length)

            for i in 0 .. expected.Length - 1 do
                Assert.AreEqual(expected.[i], actual.[i])

    let floatTest (f: array<float> -> array<float>) =
        let float_test_cases = [| 3.14; 1.41; 2.71; 0.57; 4.67 |]
        let actual = f float_test_cases
        let expected = Array.sort float_test_cases
        Assert.AreEqual(expected.Length, actual.Length)

        for i in 0 .. expected.Length - 1 do
            Assert.AreEqual(expected.[i], actual.[i])

    let charTest (f: array<char> -> array<char>) =
        let char_test_cases = [| 'D'; 'A'; 'C'; 'B'; 'E' |]
        let actual = f char_test_cases
        let expected = Array.sort char_test_cases
        Assert.AreEqual(expected.Length, actual.Length)

        for i in 0 .. expected.Length - 1 do
            Assert.AreEqual(expected.[i], actual.[i])


    [<TestMethod>]
    member this.intTestBubbleSort() = intTest bubbleSort

    [<TestMethod>]
    member this.floatTestBubbleSort() = floatTest bubbleSort

    [<TestMethod>]
    member this.charTestBubbleSort() = charTest bubbleSort


    [<TestMethod>]
    member this.intTestQuickSort() = intTest quickSort

    [<TestMethod>]
    member this.floatTestQuickSort() = floatTest quickSort

    [<TestMethod>]
    member this.charTestQuickSort() = charTest quickSort


    [<TestMethod>]
    member this.intTestMergeSort() = intTest mergeSort

    [<TestMethod>]
    member this.floatTestMergeSort() = floatTest mergeSort

    [<TestMethod>]
    member this.charTestMergeSort() = charTest mergeSort

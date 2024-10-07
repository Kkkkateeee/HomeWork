namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Library.bubble
open Library.quick


[<TestClass>]


type sorts_test() =
    let int_test_cases =
        [  []
           [ 1; 2; 3; 4; 5 ]
           [ 5; 4; 3; 2; 1 ]
           [ 3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5 ]
           [ 4; 2; 2; 8; 3; 3; 1 ] ]

    let float_test_cases = [ 3.14; 1.41; 2.71; 0.57; 4.67 ]
    let char_test_cases = [ 'D'; 'A'; 'C'; 'B'; 'E' ]

    [<TestMethod>]
    member this.TestBubbleSort_int() =
        for elem in int_test_cases do
            let res = bubble_sort elem
            let expected = List.sort elem
            Assert.AreEqual(expected, res)

    [<TestMethod>]
    member this.TestBubbleSort_float() =
        let res = bubble_sort float_test_cases
        let expected = List.sort float_test_cases
        Assert.AreEqual(expected, res)

    [<TestMethod>]
    member this.TestBubbleSort_char() =
        let res = bubble_sort char_test_cases
        let expected = List.sort char_test_cases
        Assert.AreEqual(expected, res)


    [<TestMethod>]
    member this.TestQuickSort_int() =
        for elem in int_test_cases do
            let res = quick_sort elem
            let expected = List.sort elem
            Assert.AreEqual(expected, res)

    [<TestMethod>]
    member this.TestQuickSort_float() =
        let res = quick_sort float_test_cases
        let expected = List.sort float_test_cases
        Assert.AreEqual(expected, res)

    [<TestMethod>]
    member this.TestQuickSort_char() =
        let res = quick_sort char_test_cases
        let expected = List.sort char_test_cases
        Assert.AreEqual(expected, res)


//     [<TestMethod>]
//     member this.TestMergeSort_int() =
//         for elem in int_test_cases do
//             let res = merge_sort elem
//             let expected = Array.sort elem
//             Assert.AreEqual(expected, res)

//     [<TestMethod>]
//     member this.TestMergeSort_float() =
//         let res = merge_sort float_test_cases
//         let expected = Array.sort float_test_cases
//         Assert.AreEqual(expected, res)

//     [<TestMethod>]
//     member this.TestMergeSort_char() =
//         let res = merge_sort char_test_cases
//         let expected = Array.sort char_test_cases
//         Assert.AreEqual(expected, res)
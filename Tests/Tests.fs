// namespace Tests

// open System
// open Microsoft.VisualStudio.TestTools.UnitTesting

// open Library.factorial
// open Library.sorts
// open Library.fibinacci


// [<TestClass>]

// type factorial_tests() =

//     [<TestMethod>] // 0!
//     member this.TestFactorial_Zero() = Assert.AreEqual(1, factorial (0))

//     [<TestMethod>] // 1!
//     member this.TestFactorial_One() = Assert.AreEqual(1, factorial (1))

//     [<TestMethod>] // 5!
//     member this.TestFactorial_Five() = Assert.AreEqual(120, factorial (5))

//     [<TestMethod>] // (-1)!
//     member this.TestFactorial_negative() =
//         let ex = Assert.ThrowsException<System.Exception>(fun () -> factorial -1 :> obj)
//         Assert.AreEqual("The factorial of negative numbers is not calculated by this program\n", ex.Message)


// type sorts_test() =
//     let int_test_cases =
//         [| [||]
//            [| 1; 2; 3; 4; 5 |]
//            [| 5; 4; 3; 2; 1 |]
//            [| 3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5 |]
//            [| 4; 2; 2; 8; 3; 3; 1 |] |]

//     let float_test_cases = [| 3.14; 1.41; 2.71; 0.57; 4.67 |]
//     let char_test_cases = [| 'D'; 'A'; 'C'; 'B'; 'E' |]

//     [<TestMethod>]
//     member this.TestBubbleSort_int() =
//         for elem in int_test_cases do
//             let res = bubble_sort elem
//             let expected = Array.sort elem
//             Assert.AreEqual(expected, res)

//     [<TestMethod>]
//     member this.TestBubbleSort_float() =
//         let res = bubble_sort float_test_cases
//         let expected = Array.sort float_test_cases
//         Assert.AreEqual(expected, res)

//     [<TestMethod>]
//     member this.TestBubbleSort_char() =
//         let res = bubble_sort char_test_cases
//         let expected = Array.sort char_test_cases
//         Assert.AreEqual(expected, res)


//     [<TestMethod>]
//     member this.TestQuickSort_int() =
//         for elem in int_test_cases do
//             let res = quick_sort elem
//             let expected = Array.sort elem
//             Assert.AreEqual(expected, res)

//     [<TestMethod>]
//     member this.TestQuickSort_float() =
//         let res = quick_sort float_test_cases
//         let expected = Array.sort float_test_cases
//         Assert.AreEqual(expected, res)

//     [<TestMethod>]
//     member this.TestQuickSort_char() =
//         let res = quick_sort char_test_cases
//         let expected = Array.sort char_test_cases
//         Assert.AreEqual(expected, res)


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


// type fibinacci_matrix_tests() =

//     [<TestMethod>] // F0
//     member this.Fib_0() = Assert.AreEqual(0L, fibonacci (0))

//     [<TestMethod>] // F1
//     member this.Fib_1() = Assert.AreEqual(1L, fibonacci (1))

//     [<TestMethod>] // F2
//     member this.Fib_2() = Assert.AreEqual(1L, fibonacci (2))

//     [<TestMethod>] // F3
//     member this.Fib_3() = Assert.AreEqual(2L, fibonacci (3))

//     [<TestMethod>] // F30
//     member this.Fib_30() =
//         Assert.AreEqual(832040L, fibonacci (30))

//     [<TestMethod>] // F(-1)
//     member this.Fib_negative() =
//         let ex = Assert.ThrowsException<System.Exception>(fun () -> fibonacci -1 :> obj)
//         Assert.AreEqual("Fibonacci numbers for negative n are not defined\n", ex.Message)
//
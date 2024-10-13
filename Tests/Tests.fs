namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Library.factorial
open Library.sorts
open Library.fibinacci


[<TestClass>]

type factorial_tests() =

    [<TestMethod>] // 0!
    member this.TestFactorial_Zero() = Assert.AreEqual(1, factorial (0))

    [<TestMethod>] // 1!
    member this.TestFactorial_One() = Assert.AreEqual(1, factorial (1))

    [<TestMethod>] // 5!
    member this.TestFactorial_Five() = Assert.AreEqual(120, factorial (5))

    [<TestMethod>] // (-1)!
    member this.TestFactorial_negative() =
        let ex = Assert.ThrowsException<System.Exception>(fun () -> factorial -1 :> obj)
        Assert.AreEqual("The factorial of negative numbers is not calculated by this program\n", ex.Message)


[<TestClass>]

type sorts_test() =
    let intTest (f: array<int> -> array<int>) =   
        let int_test_cases =
            [| 
                [||]
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


[<TestClass>]

type fibinacci_matrix_tests() =

    [<TestMethod>] // F0
    member this.Fib_0() = Assert.AreEqual(0L, fibonacci (0))

    [<TestMethod>] // F1
    member this.Fib_1() = Assert.AreEqual(1L, fibonacci (1))

    [<TestMethod>] // F2
    member this.Fib_2() = Assert.AreEqual(1L, fibonacci (2))

    [<TestMethod>] // F3
    member this.Fib_3() = Assert.AreEqual(2L, fibonacci (3))

    [<TestMethod>] // F30
    member this.Fib_30() =
        Assert.AreEqual(832040L, fibonacci (30))

    [<TestMethod>] // F(-1)
    member this.Fib_negative() =
        let ex = Assert.ThrowsException<System.Exception>(fun () -> fibonacci -1 :> obj)
        Assert.AreEqual("Fibonacci numbers for negative n are not defined\n", ex.Message)
        
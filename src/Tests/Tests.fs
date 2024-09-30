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
    member this.Factorial_negative() =
        let ex = Assert.ThrowsException<System.Exception>(fun () -> factorial -1 :> obj)
        Assert.AreEqual("The factorial of negative numbers is not calculated by this program\n", ex.Message)


type bubble_sort_tests() =

    [<TestMethod>] // empty array
    member this.TestEmpty() =
        let arr = [||]
        bubble_sort arr
        Assert.IsTrue(Seq.forall2 (=) arr [||])

    [<TestMethod>] // sorted array
    member this.TestSortedArray() =
        let arr = [| 1; 2; 3; 4; 5 |]
        bubble_sort arr
        Assert.IsTrue(Seq.forall2 (=) arr [| 1; 2; 3; 4; 5 |])

    [<TestMethod>] // reverse sorted array
    member this.TestReverseSortedArray() =
        let arr = [| 5; 4; 3; 2; 1 |]
        bubble_sort arr
        Assert.IsTrue(Seq.forall2 (=) arr [| 1; 2; 3; 4; 5 |])

    [<TestMethod>] // unsorted array
    member this.TestUnsortedArray() =
        let arr = [| 3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5 |]
        bubble_sort arr
        Assert.IsTrue(Seq.forall2 (=) arr [| 1; 1; 2; 3; 3; 4; 5; 5; 5; 6; 9 |])

    [<TestMethod>] // array with repeating elements
    member this.TestArrayWithDuplicates() =
        let arr = [| 4; 2; 2; 8; 3; 3; 1 |]
        bubble_sort arr
        Assert.IsTrue(Seq.forall2 (=) arr [| 1; 2; 2; 3; 3; 4; 8 |])

    [<TestMethod>] // float array
    member this.TestFloatArray() =
        let arr = [| 3.14; 1.41; 2.71; 0.57; 4.67 |]
        bubble_sort arr
        Assert.IsTrue(Seq.forall2 (=) arr [| 0.57; 1.41; 2.71; 3.14; 4.67 |])

    [<TestMethod>] // float array
    member this.TestCharArray() =
        let arr = [| 'D'; 'A'; 'C'; 'B'; 'E' |]
        bubble_sort arr
        Assert.IsTrue(Seq.forall2 (=) arr [| 'A'; 'B'; 'C'; 'D'; 'E' |])


type quick_sort_tests() =

    [<TestMethod>] // empty array
    member this.Empty() =
        let arr = [||]
        quick_sort arr 0 0
        Assert.IsTrue(Seq.forall2 (=) arr [||])

    [<TestMethod>] // sorted array
    member this.SortedArray() =
        let arr = [| 1; 2; 3; 4; 5 |]
        quick_sort arr 0 4
        Assert.IsTrue(Seq.forall2 (=) arr [| 1; 2; 3; 4; 5 |])

    [<TestMethod>] // reverse sorted array
    member this.TestReverseSortedArray() =
        let arr = [| 5; 4; 3; 2; 1 |]
        quick_sort arr 0 4
        Assert.IsTrue(Seq.forall2 (=) arr [| 1; 2; 3; 4; 5 |])

    [<TestMethod>] // unsorted array
    member this.TestUnsortedArray() =
        let arr = [| 3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5 |]
        quick_sort arr 0 10
        Assert.IsTrue(Seq.forall2 (=) arr [| 1; 1; 2; 3; 3; 4; 5; 5; 5; 6; 9 |])

    [<TestMethod>] // array with repeating
    member this.TestArrayWithDuplicates() =
        let arr = [| 4; 2; 2; 8; 3; 3; 1 |]
        quick_sort arr 0 6
        Assert.IsTrue(Seq.forall2 (=) arr [| 1; 2; 2; 3; 3; 4; 8 |])

    [<TestMethod>] // float array
    member this.TestFloatArray() =
        let arr = [| 3.14; 1.41; 2.71; 0.57; 4.67 |]
        quick_sort arr 0 4
        Assert.IsTrue(Seq.forall2 (=) arr [| 0.57; 1.41; 2.71; 3.14; 4.67 |])

    [<TestMethod>] // float array
    member this.TestCharArray() =
        let arr = [| 'D'; 'A'; 'C'; 'B'; 'E' |]
        quick_sort arr 0 4
        Assert.IsTrue(Seq.forall2 (=) arr [| 'A'; 'B'; 'C'; 'D'; 'E' |])


type merge_sort_tests() =

    [<TestMethod>] // empty array
    member this.Empty() =
        let arr = [||]
        merge_sort arr 0 0
        Assert.IsTrue(Seq.forall2 (=) arr [||])

    [<TestMethod>] // sorted array
    member this.SortedArray() =
        let arr = [| 1; 2; 3; 4; 5 |]
        merge_sort arr 0 4
        Assert.IsTrue(Seq.forall2 (=) arr [| 1; 2; 3; 4; 5 |])

    [<TestMethod>] // reverse sorted array
    member this.TestReverseSortedArray() =
        let arr = [| 5; 4; 3; 2; 1 |]
        merge_sort arr 0 4
        Assert.IsTrue(Seq.forall2 (=) arr [| 1; 2; 3; 4; 5 |])

    [<TestMethod>] // unsorted array
    member this.TestUnsortedArray() =
        let arr = [| 3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5 |]
        merge_sort arr 0 10
        Assert.IsTrue(Seq.forall2 (=) arr [| 1; 1; 2; 3; 3; 4; 5; 5; 5; 6; 9 |])

    [<TestMethod>] // array with repeating
    member this.TestArrayWithDuplicates() =
        let arr = [| 4; 2; 2; 8; 3; 3; 1 |]
        merge_sort arr 0 6
        Assert.IsTrue(Seq.forall2 (=) arr [| 1; 2; 2; 3; 3; 4; 8 |])

    [<TestMethod>] // float array
    member this.TestFloatArray() =
        let arr = [| 3.14; 1.41; 2.71; 0.57; 4.67 |]
        merge_sort arr 0 4
        Assert.IsTrue(Seq.forall2 (=) arr [| 0.57; 1.41; 2.71; 3.14; 4.67 |])

    [<TestMethod>] // float array
    member this.TestCharArray() =
        let arr = [| 'D'; 'A'; 'C'; 'B'; 'E' |]
        merge_sort arr 0 4
        Assert.IsTrue(Seq.forall2 (=) arr [| 'A'; 'B'; 'C'; 'D'; 'E' |])


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

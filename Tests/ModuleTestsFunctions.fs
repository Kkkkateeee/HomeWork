namespace ModuleTestsFunctions

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Functions.factorial
open Functions.fibinacci


[<TestClass>]

type factorial_tests() =

    [<TestMethod>] // 0!
    member this.TestFactorial_Zero() = Assert.AreEqual<int>(1, factorial (0))

    [<TestMethod>] // 1!
    member this.TestFactorial_One() = Assert.AreEqual<int>(1, factorial (1))

    [<TestMethod>] // 5!
    member this.TestFactorial_Five() =
        Assert.AreEqual<int>(120, factorial (5))

    [<TestMethod>] // (-1)!
    member this.TestFactorial_negative() =
        let ex = Assert.ThrowsException<System.Exception>(fun () -> factorial -1 :> obj)
        Assert.AreEqual<string>("The factorial of negative numbers is not calculated by this program\n", ex.Message)


[<TestClass>]

type fibinacci_matrix_tests() =

    [<TestMethod>] // F0
    member this.Fib_0() =
        Assert.AreEqual<int64>(0L, fibonacci (0))

    [<TestMethod>] // F1
    member this.Fib_1() =
        Assert.AreEqual<int64>(1L, fibonacci (1))

    [<TestMethod>] // F2
    member this.Fib_2() =
        Assert.AreEqual<int64>(1L, fibonacci (2))

    [<TestMethod>] // F3
    member this.Fib_3() =
        Assert.AreEqual<int64>(2L, fibonacci (3))

    [<TestMethod>] // F30
    member this.Fib_30() =
        Assert.AreEqual<int64>(832040L, fibonacci (30))

    [<TestMethod>] // F(-1)
    member this.Fib_negative() =
        let ex = Assert.ThrowsException<System.Exception>(fun () -> fibonacci -1 :> obj)
        Assert.AreEqual<string>("Fibonacci numbers for negative n are not defined\n", ex.Message)

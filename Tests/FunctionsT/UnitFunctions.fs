namespace UnitTestsFunctions

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Functions.Factorial
open Functions.Fibinacci


[<TestClass>]

type FactorialTests() =

    [<TestMethod>] 
    member this.testFactorialZero() = Assert.AreEqual(1, factorial (0))

    [<TestMethod>] 
    member this.testFactorialOne() = Assert.AreEqual(1, factorial (1))

    [<TestMethod>] 
    member this.testFactorialFive() = Assert.AreEqual(120, factorial (5))

    [<TestMethod>] 
    member this.testFactorialNegative() =
        let ex = Assert.ThrowsException<System.Exception>(fun () -> factorial -1 :> obj)
        Assert.AreEqual("The factorial of negative numbers is not calculated by this program\n", ex.Message)


[<TestClass>]

type FibinacciMatrixTests() =

    [<TestMethod>] 
    member this.fibZero() = Assert.AreEqual(0L, fibonacci (0))

    [<TestMethod>] 
    member this.fibOne() = Assert.AreEqual(1L, fibonacci (1))

    [<TestMethod>] 
    member this.fibTwo() = Assert.AreEqual(1L, fibonacci (2))

    [<TestMethod>] 
    member this.fibThree() = Assert.AreEqual(2L, fibonacci (3))

    [<TestMethod>] 
    member this.fibThreety() =
        Assert.AreEqual(832040L, fibonacci (30))

    [<TestMethod>] 
    member this.fibNegative() =
        let ex = Assert.ThrowsException<System.Exception>(fun () -> fibonacci -1 :> obj)
        Assert.AreEqual("Fibonacci numbers for negative n are not defined\n", ex.Message)

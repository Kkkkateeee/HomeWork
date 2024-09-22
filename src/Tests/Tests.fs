namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Library.Say  // подключаем пространство имен


[<TestClass>]
type TestClass () =

    [<TestMethod>]  // 0!
    member this.TestFactorial_Zero () =
        Assert.AreEqual(1, factorial(0))

    [<TestMethod>]  // 1!
    member this.TestFactorial_One () =
        Assert.AreEqual(1, factorial(1))

    [<TestMethod>]  // 5!
    member this.TestFactorial_Five () =
        Assert.AreEqual(120, factorial(5))

    [<TestMethod>]  // (-1)!
    member this.Factorial_negative () =
        let ex = Assert.ThrowsException<System.Exception>(fun () -> factorial -1 :> obj)
        Assert.AreEqual("Факториал отрицательных чисел не вычисляется этой программой\n", ex.Message)
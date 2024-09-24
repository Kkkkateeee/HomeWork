namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Library.fibinacci  // add namespace


[<TestClass>]
type TestClass () =

    [<TestMethod>]  // F0
    member this.Fib_0 () =
        Assert.AreEqual(0L, fibonacci(0))
    
    [<TestMethod>]  // F1
    member this.Fib_1 () =
        Assert.AreEqual(1L, fibonacci(1))
    
    [<TestMethod>]  // F2
    member this.Fib_2 () =
        Assert.AreEqual(1L, fibonacci(2))
    
    [<TestMethod>]  // F3
    member this.Fib_3 () =
        Assert.AreEqual(2L, fibonacci(3))
    
    [<TestMethod>]  // F30
    member this.Fib_30 () =
        Assert.AreEqual(832040L, fibonacci(30))

    [<TestMethod>]  // F(-1)
    member this.Fib_negative () =
        let ex = Assert.ThrowsException<System.Exception>(fun () -> fibonacci -1 :> obj)
        Assert.AreEqual("Fibonacci numbers for negative n are not defined\n", ex.Message)
    

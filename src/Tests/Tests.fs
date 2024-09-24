namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Library.bubble_sort  // add namespace


[<TestClass>]
type TestClass () =

    [<TestMethod>]  // empty array
    member this.TestEmpty() =
        let arr = [| |]
        bubble_sort arr
        Assert.IsTrue(Seq.forall2 (=) arr [| |])

    [<TestMethod>]  // sorted array
    member this.TestSortedArray() =
        let arr = [|1; 2; 3; 4; 5|]
        bubble_sort arr
        Assert.IsTrue(Seq.forall2 (=) arr [|1; 2; 3; 4; 5|])

    [<TestMethod>]  // reverse sorted array
    member this.TestReverseSortedArray() =
        let arr = [|5; 4; 3; 2; 1|]
        bubble_sort arr
        Assert.IsTrue(Seq.forall2 (=) arr [|1; 2; 3; 4; 5|])
        
    [<TestMethod>]  // unsorted array
    member this.TestUnsortedArray() =
        let arr = [|3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5|]
        bubble_sort arr
        Assert.IsTrue(Seq.forall2 (=) arr [|1; 1; 2; 3; 3; 4; 5; 5; 5; 6; 9|])

    [<TestMethod>]  // array with repeating elements
    member this.TestArrayWithDuplicates() =
        let arr = [|4; 2; 2; 8; 3; 3; 1|]
        bubble_sort arr
        Assert.IsTrue(Seq.forall2 (=) arr [|1; 2; 2; 3; 3; 4; 8|])

namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Library.quick_sort  // подключаем пространство имен


[<TestClass>]
type TestClass () =

    [<TestMethod>]  // пустой массив
    member this.Empty () =
        let arr = [| |]
        let sorted_arr = Quick_sort arr
        Assert.IsTrue(Seq.forall2 (=) sorted_arr [| |])
        
    [<TestMethod>]  // отсортированный массив
    member this.SortedArray () =
        let arr = [|1; 2; 3; 4; 5|]
        let sorted_arr = Quick_sort arr
        Assert.IsTrue(Seq.forall2 (=) sorted_arr [|1; 2; 3; 4; 5|])

    [<TestMethod>]  // массив обратной сортироваки
    member this.TestReverseSortedArray() =
        let arr = [|5; 4; 3; 2; 1|]
        let sorted_arr = Quick_sort arr
        Assert.IsTrue(Seq.forall2 (=) sorted_arr [|1; 2; 3; 4; 5|])

    [<TestMethod>]  // массив произвольной сортировки
    member this.TestUnsortedArray() =
        let arr = [|3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5|]
        let sorted_arr = Quick_sort arr
        Assert.IsTrue(Seq.forall2 (=) sorted_arr [|1; 1; 2; 3; 3; 4; 5; 5; 5; 6; 9|])

    [<TestMethod>]  // массив с повторяющимися элементами
    member this.TestArrayWithDuplicates() =
        let arr = [|4; 2; 2; 8; 3; 3; 1|]
        let sorted_arr = Quick_sort arr
        Assert.IsTrue(Seq.forall2 (=) sorted_arr [|1; 2; 2; 3; 3; 4; 8|])
namespace SortingsB

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Reports
open Perfolizer.Horology

open ArraySorts
open MyListSorts

type ArraysBenchmark() =

    static member Lengths = [|10000..10000..100000|]
    member this.Random = System.Random()

    [<ParamsSource("Lengths")>]
    member val ArrayLength = 0 with get, set
    member val ArrayToSort = [|0.0|] with get, set

    [<IterationSetup>]
    member this.GetArrayToSort () = 
        this.ArrayToSort <- Array.init this.ArrayLength (fun _ -> this.Random.NextDouble()) 

    [<Benchmark>]
    member this.ArrayQuickSort () = Arrays.quickSort this.ArrayToSort

    [<Benchmark>]
    member this.ArrayBubbleSort () = Arrays.bubbleSort this.ArrayToSort

    [<Benchmark>]
    member this.ArrayMergeSort () = Arrays.mergeSort this.ArrayToSort

    [<Benchmark>]
    member this.ArraySystem () = Array.Sort this.ArrayToSort
    

type ListBenchmark() =
    
    static member Lengths = [|10000..10000..100000|]
    member this.Random = System.Random()

    [<ParamsSource("Lengths")>]
    member val ListLength = 0 with get, set
    member val ListToSort = [0.0] with get, set

    [<IterationSetup>]
    member this.GetListToSort () = 
        this.ListToSort <- List.init this.ListLength (fun _ -> this.Random.NextDouble()) 

    [<Benchmark>]
    member this.MyListBubbleSort () = MyList.fromSystemList this.ListToSort |> MyList.quickSort 

    [<Benchmark>]
    member this.MyListQuickSort () = MyList.fromSystemList this.ListToSort |> MyList.bubbleSort

    [<Benchmark>]
    member this.MyListMergeSort () = MyList.fromSystemList this.ListToSort |> MyList.mergeSort

    [<Benchmark>]
    member this.MyListSystem () = List.sort this.ListToSort 


module Main =
    [<EntryPoint>]
    let main argv =
        
        let config = ManualConfig.Create(DefaultConfig.Instance)
                        .WithSummaryStyle(SummaryStyle.Default.WithTimeUnit(TimeUnit.Millisecond)) 
                        .WithOptions(ConfigOptions.DisableOptimizationsValidator) 
        let benchmarks =
            BenchmarkSwitcher [| typeof<ArraysBenchmark>;  typeof<ListBenchmark>|]

        benchmarks.Run argv |> ignore
        0

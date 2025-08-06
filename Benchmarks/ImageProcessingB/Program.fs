// namespace ImageProcessingB

// open System
// open BenchmarkDotNet.Attributes
// open BenchmarkDotNet.Running
// open BenchmarkDotNet.Configs
// open BenchmarkDotNet.Reports
// open Perfolizer.Horology

// open ImageProcessing

// type ipBenchmark() =

//     static member sizes = [|10000..10000..100000|]
//     member this.Random = Random()

//     [<ParamsSource("sizes")>]
//     member val ArrayLength = 0 with get, set
//     member val ArrayToSort = [|0.0|] with get, set

//     [<IterationSetup>]
//     member this.GetArrayToSort () = 
//         this.ArrayToSort <- Array.init this.ArrayLength (fun _ -> this.Random.NextDouble()) 

//     [<Benchmark>]
//     member this.ArrayQuickSort () =  this.ArrayToSort

//     // [<Benchmark>]
//     // member this.ArrayBubbleSort () = Arrays.bubbleSort this.ArrayToSort

//     // [<Benchmark>]
//     // member this.ArrayMergeSort () = Arrays.mergeSort this.ArrayToSort

//     // [<Benchmark>]
//     // member this.ArraySystem () = Array.Sort this.ArrayToSort
    

// module Main =
//     [<EntryPoint>]
//     let main argv =
        
//         let config = ManualConfig.Create(DefaultConfig.Instance)
//                         .WithSummaryStyle(SummaryStyle.Default.WithTimeUnit(TimeUnit.Millisecond)) 
//                         .WithOptions(ConfigOptions.DisableOptimizationsValidator) 
//         let benchmarks =
//             BenchmarkSwitcher [| typeof<ArraysBenchmark> |]

//         benchmarks.Run argv |> ignore
//         0

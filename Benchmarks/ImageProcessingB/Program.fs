namespace ImageProcessingB

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Reports
open Perfolizer.Horology

open ImageProcessing.ImProcessing
open SixLabors.ImageSharp.PixelFormats

type ipBenchmark() =

    static member Sizes = [|10000..10000..100000|]
    member this.Random = System.Random()

    [<ParamsSource("Sizes")>]
    member val MatrixSize = 0 with get, set
    member val MatrixToSort = Array2D.zeroCreate<Rgba32> 1 1 with get, set

    [<IterationSetup>]
    member this.GetArrayToSort () = 
        this.MatrixToSort <- Array2D.init this.MatrixSize this.MatrixSize (fun _ _ -> 
            let r = byte (this.Random.Next 256)
            let g = byte (this.Random.Next 256)
            let b = byte (this.Random.Next 256)
            let a = byte (this.Random.Next 256)
            Rgba32 (r, g, b, a)
        ) 

    [<Benchmark>]
    member this.NoParallelism () = applyFilterNoParallelism gaussianBlur this.MatrixToSort

    [<Benchmark>]
    member this.PixelParallelism () = applyFilterPixelParallelism gaussianBlur this.MatrixToSort

    [<Benchmark>]
    member this.ParallelismInParts () = applyFilterParallelismInParts gaussianBlur this.MatrixToSort

    [<Benchmark>]
    member this.RowParallelism () = applyFilterRowParallelism gaussianBlur this.MatrixToSort

    [<Benchmark>]
    member this.ColParallelism () = applyFilterColParallelism gaussianBlur this.MatrixToSort


module Main =
    [<EntryPoint>]
    let main argv =
        let benchmarks =
            BenchmarkSwitcher [| typeof<ipBenchmark> |]

        benchmarks.Run argv |> ignore
        0
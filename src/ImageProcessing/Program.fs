(*
ДЗ 3: парарллельная асинхронная обработка изображений. 

Используя примитивы F# распараллелить свёртку одного изображения
0) По пикселям +
1) На "произвольные" части +
2) Строки +
3) Столбцы +

реализовать асинхронное чтение-запись при потоковой обработке нескольких изображений

анализ производительности +
*)

open System
open System.Threading
open ImageProcessing.ImProcessing
open Argu


type Filters =
    | gaussianBlur = 1
    | motionDiagonal135deg = 2  
    | motionDiagonal315deg = 3
    | motionVertical = 4
    | motionHorizontal = 5
    | edgesHorizontal = 6
    | edgesVertical = 7
    | edgesDioganal135deg = 8
    | edgesDioganal315deg = 9
    | edgesAllDirections = 10
    | sharpen = 11
    | sharpenSoft = 12
    | sharpenWithEdges = 13
    | emboss = 14
    | embossHard = 15

type ParallelismTypes =
    | noParallelism = 0
    | pixelParallelism = 1
    | parallelismInParts = 2
    | rowParallelism = 3
    | colParallelism = 4


type Arguments =
    | [<Mandatory>] Input_File of string
    | [<Mandatory>] Out_File of string
    | [<Mandatory>] Filters of list<Filters>
    | [<Mandatory>] Parallelism of list<ParallelismTypes>
    interface IArgParserTemplate with
        member this.Usage =
            match this with 
            | Input_File _ -> "File to process."
            | Out_File _ -> "Result name."
            | Filters _ -> "Which filters to apply (comma-separated)."
            | Parallelism _ -> "Type of parallelism."


[<EntryPoint>]
let main argv = 
    
    let parser = ArgumentParser.Create<Arguments>(programName = "ImageProcessing")  
    let results = parser.Parse argv  

    let inFile = results.GetResult Input_File  
    let outFile = results.GetResult Out_File
    let filters = results.GetResult Filters
    let parallelisms = results.GetResult Parallelism

    let inImage = loadAsRgba2D inFile 

    let applyFilterWithParallelism filter img parallelism =
        match parallelism with
        | ParallelismTypes.noParallelism -> applyFilterNoParallelism filter img
        | ParallelismTypes.pixelParallelism -> applyFilterPixelParallelism filter img
        | ParallelismTypes.parallelismInParts -> applyFilterParallelismInParts filter img
        | ParallelismTypes.rowParallelism -> applyFilterRowParallelism filter img
        | ParallelismTypes.colParallelism -> applyFilterColParallelism filter img
        | _ -> 
            printfn "Unknown parallelism type"
            img

    let resultImage =
        List.zip filters parallelisms |> List.fold (fun img (filter, parallelism) ->
            match filter with 
            | Filters.gaussianBlur -> applyFilterWithParallelism gaussianBlur img parallelism 
            | Filters.motionDiagonal135deg -> applyFilterWithParallelism motionDiagonal135deg img parallelism 
            | Filters.motionDiagonal315deg -> applyFilterWithParallelism motionDiagonal315deg img parallelism 
            | Filters.motionVertical -> applyFilterWithParallelism motionVertical img parallelism 
            | Filters.motionHorizontal -> applyFilterWithParallelism motionHorizontal img parallelism 
            | Filters.edgesHorizontal -> applyFilterWithParallelism edgesHorizontal img parallelism 
            | Filters.edgesVertical -> applyFilterWithParallelism edgesVertical img parallelism 
            | Filters.edgesDioganal135deg -> applyFilterWithParallelism edgesDioganal135deg img parallelism 
            | Filters.edgesDioganal315deg -> applyFilterWithParallelism edgesDioganal315deg img parallelism 
            | Filters.edgesAllDirections -> applyFilterWithParallelism edgesAllDirections img parallelism 
            | Filters.sharpen -> applyFilterWithParallelism sharpen img parallelism 
            | Filters.sharpenSoft -> applyFilterWithParallelism sharpenSoft img parallelism 
            | Filters.sharpenWithEdges -> applyFilterWithParallelism sharpenWithEdges img parallelism 
            | Filters.emboss -> applyFilterWithParallelism emboss img parallelism 
            | Filters.embossHard -> applyFilterWithParallelism embossHard img parallelism 
            | _ -> 
                printfn "Unknown filter"
                img
        ) inImage
    
    saveRgbaImage resultImage outFile
    
    0
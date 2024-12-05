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

type Arguments =
    | [<Mandatory>] Input_File of string
    | [<Mandatory>] Out_File of string
    | [<Mandatory>] Filters of list<Filters>
    interface IArgParserTemplate with
        member this.Usage =
            match this with 
            | Input_File _ -> "File to process."
            | Out_File _ -> "Where to save result."
            | Filters _ -> "Which filters to apply (comma-separated)."


[<EntryPoint>]
let main argv = 
    
    let parser = ArgumentParser.Create<Arguments>(programName = "ImageProcessing")  
    let results = parser.Parse argv  

    let inFile = results.GetResult Input_File  
    let outFile = results.GetResult Out_File
    let filters = results.GetResult Filters

    let inImage = loadAsRgba2D inFile 

    let resultImage =
        filters |> List.fold (fun img filter ->
            match filter with 
            | Filters.gaussianBlur -> applyFilter gaussianBlur img 
            | Filters.motionDiagonal135deg -> applyFilter motionDiagonal135deg img 
            | Filters.motionDiagonal315deg -> applyFilter motionDiagonal315deg img 
            | Filters.motionVertical -> applyFilter motionVertical img 
            | Filters.motionHorizontal -> applyFilter motionHorizontal img 
            | Filters.edgesHorizontal -> applyFilter edgesHorizontal img 
            | Filters.edgesVertical -> applyFilter edgesVertical img 
            | Filters.edgesDioganal135deg -> applyFilter edgesDioganal135deg img 
            | Filters.edgesDioganal315deg -> applyFilter edgesDioganal315deg img 
            | Filters.edgesAllDirections -> applyFilter edgesAllDirections img 
            | Filters.sharpen -> applyFilter sharpen img 
            | Filters.sharpenSoft -> applyFilter sharpenSoft img 
            | Filters.sharpenWithEdges -> applyFilter sharpenWithEdges img 
            | Filters.emboss -> applyFilter emboss img 
            | Filters.embossHard -> applyFilter embossHard img 
            | _ -> 
                printfn "Unknown filter"
                img
                
        ) inImage
    
    saveRgbaImage resultImage outFile
    
    0
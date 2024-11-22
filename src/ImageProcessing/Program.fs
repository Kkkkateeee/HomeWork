open ImageProcessing.IProcessing
open System
open Argu

type CmdArgs =
    | [<Mandatory>] Input_File of string
    | [<Mandatory>] Out_File of string
    | [<Mandatory>] Filter of string
    interface IArgParserTemplate with
        member this.Usage =
            match this with 
            | Input_File _ -> "File to process."
            | Out_File _ -> "Where to save result."
            | Filter _ -> "Which filters to apply (comma-separated)."

[<EntryPoint>]
let main argv = 
    
    let parser = ArgumentParser.Create<CmdArgs>(programName = "ImageProcessing")  
    let results = parser.Parse argv  

    let inFile = results.GetResult Input_File  
    let outFile = results.GetResult Out_File
    let filters = results.GetResult Filter

    let inImage = loadAsRgba2D inFile 

    let filterList = filters.Split(',') |> Array.map (fun f -> f.Trim())

    let resultImage =
        filterList |> Array.fold (fun img filter ->
            match filter with 
            | "gaussianBlur" -> applyFilter gaussianBlur img 
            | "motionDiagonal135deg" -> applyFilter motionDiagonal135deg img 
            | "motionDiagonal315deg" -> applyFilter motionDiagonal315deg img 
            | "motionVertical" -> applyFilter motionVertical img 
            | "motionHorizontal" -> applyFilter motionHorizontal img 
            | "edgesHorizontal" -> applyFilter edgesHorizontal img 
            | "edgesVertical" -> applyFilter edgesVertical img 
            | "edgesDioganal135deg" -> applyFilter edgesDioganal135deg img 
            | "edgesDioganal315deg" -> applyFilter edgesDioganal315deg img 
            | "edgesAllDirections" -> applyFilter edgesAllDirections img 
            | "sharpen" -> applyFilter sharpen img 
            | "sharpenSoft" -> applyFilter sharpenSoft img 
            | "sharpenWithEdges" -> applyFilter sharpenWithEdges img 
            | "emboss" -> applyFilter emboss img 
            | "embossHard" -> applyFilter embossHard img 
            | _ -> 
                printfn "Unknown filter: %s" filter
                img 
        ) inImage

    
    saveRgbaImage resultImage outFile
    
    0

open System
open System.Threading.Tasks
open System.IO
open ImageProcessing.ImProcessing
open Argu
open SixLabors.ImageSharp.PixelFormats

type Filters =
    | GaussianBlur
    | MotionDiagonal135deg 
    | MotionDiagonal315deg
    | MotionVertical 
    | MotionHorizontal 
    | EdgesHorizontal 
    | EdgesVertical 
    | EdgesDioganal135deg 
    | EdgesDioganal315deg 
    | EdgesAllDirections 
    | Sharpen 
    | SharpenSoft 
    | SharpenWithEdges 
    | Emboss 
    | EmbossHard 


type ParallelismTypes =
    | NoParallelism 
    | PixelParallelism 
    | ParallelismInParts 
    | RowParallelism 
    | ColParallelism 


type Arguments =
    | [<Mandatory>] Input_Folder of string
    | [<Mandatory>] Out_Folder of string
    | [<Mandatory>] Filters of list<Filters>
    | [<Mandatory>] Parallelism of list<ParallelismTypes>
    interface IArgParserTemplate with
        member this.Usage =
            match this with 
            | Input_Folder _ -> "Folder with images for processing."
            | Out_Folder _ -> "Folder for processed images."
            | Filters _ -> "Which filters to apply (comma-separated)."
            | Parallelism _ -> "Type of parallelism."


type FileProcessorMessage =
    | ProcessImage of string * string * list<Filters> * list<ParallelismTypes> * AsyncReplyChannel<Result<unit, string>> 
    | Stop


let createFileProcessor () =
    let getParallelismType (parallelism: list<ParallelismTypes>) (filter: float32[][]) (img: Async<Rgba32[,]>) =
        match parallelism.[0] with 
        | NoParallelism -> applyFilterColParallelismA filter img
        | PixelParallelism  -> applyFilterPixelParallelismA filter img
        | ParallelismInParts -> applyFilterParallelismInPartsA filter img
        | RowParallelism -> applyFilterRowParallelismA filter img
        | ColParallelism -> applyFilterColParallelismA filter img

    MailboxProcessor.Start(fun inbox ->

        let rec loop () = 
            async {
                let! msg = inbox.Receive()

                match msg with
                | ProcessImage (sourcePath, destinationPath, filters, parallelism, replyChannel) ->
                    try
                        let imageData = loadAsRgba2DA sourcePath
                        let! filteredImage =
                            filters |> List.fold (fun img filter ->
                                match filter with 
                                | GaussianBlur -> getParallelismType parallelism gaussianBlur img 
                                | MotionDiagonal135deg -> getParallelismType parallelism motionDiagonal135deg img 
                                | MotionDiagonal315deg -> getParallelismType parallelism motionDiagonal315deg img 
                                | MotionVertical -> getParallelismType parallelism motionVertical img 
                                | MotionHorizontal -> getParallelismType parallelism motionHorizontal img 
                                | EdgesHorizontal -> getParallelismType parallelism edgesHorizontal img 
                                | EdgesVertical -> getParallelismType parallelism edgesVertical img 
                                | EdgesDioganal135deg -> getParallelismType parallelism edgesDioganal135deg img 
                                | EdgesDioganal315deg -> getParallelismType parallelism edgesDioganal315deg img 
                                | EdgesAllDirections -> getParallelismType parallelism edgesAllDirections img 
                                | Sharpen -> getParallelismType parallelism sharpen img 
                                | SharpenSoft -> getParallelismType parallelism sharpenSoft img 
                                | SharpenWithEdges -> getParallelismType parallelism sharpenWithEdges img 
                                | Emboss -> getParallelismType parallelism emboss img 
                                | EmbossHard -> getParallelismType parallelism embossHard img 
                                    
                            ) imageData 

                        do! saveRgbaImageA (async.Return filteredImage) destinationPath
                        replyChannel.Reply(Ok ())
                        return! loop ()
                    with
                    | ex ->
                        replyChannel.Reply(Error ex.Message)
                        return! loop ()

                | Stop ->
                    return ()
            }
        loop ()
    )


[<EntryPoint>]
let main argv = 

    let fileProcessor = createFileProcessor ()

    let processImageAsync (sourcePath: string) (destinationPath: string) (filters: list<Filters>) (parallelism: list<ParallelismTypes>) =
        async {
            let! reply = fileProcessor.PostAndAsyncReply (fun replyChannel -> ProcessImage(sourcePath, destinationPath, filters, parallelism, replyChannel))
            return reply
        }

    let parser = ArgumentParser.Create<Arguments>(programName = "ImageProcessing")  
    let results = parser.Parse argv  

    let inFolder = results.GetResult Input_Folder
    let outFolder = results.GetResult Out_Folder
    let filters = results.GetResult Filters
    let parallelisms = results.GetResult Parallelism


    if not (Directory.Exists inFolder) then
        printfn $"Error: Input folder '{inFolder}' does not exist."
        1 
    else
        let imageFiles =
            try
                Directory.GetFiles(inFolder, "*.*")
                |> Array.filter (fun file ->
                    let extension = Path.GetExtension file
                    extension.ToLower() = ".jpg" || extension.ToLower() = ".jpeg" || extension.ToLower() = ".png" || extension.ToLower() = ".bmp")
                |> Array.toList
            with ex ->
                printfn $"Error getting image files from '{inFolder}': {ex.Message}"
                [] 

        if imageFiles.IsEmpty then
            printfn $"Error: No images found in input folder '{inFolder}'."
            1 
        else
            Directory.CreateDirectory outFolder |> ignore

            let processingTasks =
                imageFiles
                |> List.map (fun sourcePath ->
                    let filename = Path.GetFileName sourcePath
                    let destinationPath = Path.Combine(outFolder, filename)
                    processImageAsync sourcePath destinationPath filters parallelisms
                )   

            Async.AwaitTask (Task.WhenAll(processingTasks |> List.map Async.StartAsTask |> List.toArray)) |> Async.RunSynchronously |> ignore

            fileProcessor.Post Stop

            printfn "Processing complete."

            0 
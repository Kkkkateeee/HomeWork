open System
open System.Threading.Tasks
open System.IO
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
    | ProcessImage of string * string * float32[][] * AsyncReplyChannel<Result<unit, string>> 
    | Stop


let createFileProcessor () =
    MailboxProcessor.Start(fun inbox ->

        let rec loop () = 
            async {
                let! msg = inbox.Receive()

                match msg with
                | ProcessImage (sourcePath, destinationPath, filter, replyChannel) ->
                    try
                        let! imageData = loadAsRgba2DA sourcePath
                        let! filteredImage = applyFilterNoParallelismA filter (async.Return imageData)
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

    let processImageAsync (sourcePath: string) (destinationPath: string) (filter: float32[][]) =
        async {
            let! reply = fileProcessor.PostAndAsyncReply (fun replyChannel -> ProcessImage(sourcePath, destinationPath, filter, replyChannel))
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
                    processImageAsync sourcePath destinationPath gaussianBlur
                )

            Async.AwaitTask (Task.WhenAll(processingTasks |> List.map Async.StartAsTask |> List.toArray)) |> Async.RunSynchronously |> ignore

            fileProcessor.Post Stop

            printfn "Processing complete."



            // let inImage = loadAsRgba2DA inFolder

            // let applyFilterWithParallelism filter img parallelism =
            //     match parallelism with
            //     | ParallelismTypes.noParallelism -> applyFilterNoParallelismA filter img
            //     | ParallelismTypes.pixelParallelism -> applyFilterPixelParallelismA filter img
            //     | ParallelismTypes.parallelismInParts -> applyFilterParallelismInPartsA filter img
            //     | ParallelismTypes.rowParallelism -> applyFilterRowParallelismA filter img
            //     | ParallelismTypes.colParallelism -> applyFilterColParallelismA filter img
            //     | _ -> 
            //         printfn "Unknown parallelism type"
            //         img

            // let resultImage =
            //     List.zip filters parallelisms |> List.fold (fun img (filter, parallelism) ->
            //         match filter with 
            //         | Filters.gaussianBlur -> applyFilterWithParallelism gaussianBlur img parallelism 
            //         | Filters.motionDiagonal135deg -> applyFilterWithParallelism motionDiagonal135deg img parallelism 
            //         | Filters.motionDiagonal315deg -> applyFilterWithParallelism motionDiagonal315deg img parallelism 
            //         | Filters.motionVertical -> applyFilterWithParallelism motionVertical img parallelism 
            //         | Filters.motionHorizontal -> applyFilterWithParallelism motionHorizontal img parallelism 
            //         | Filters.edgesHorizontal -> applyFilterWithParallelism edgesHorizontal img parallelism 
            //         | Filters.edgesVertical -> applyFilterWithParallelism edgesVertical img parallelism 
            //         | Filters.edgesDioganal135deg -> applyFilterWithParallelism edgesDioganal135deg img parallelism 
            //         | Filters.edgesDioganal315deg -> applyFilterWithParallelism edgesDioganal315deg img parallelism 
            //         | Filters.edgesAllDirections -> applyFilterWithParallelism edgesAllDirections img parallelism 
            //         | Filters.sharpen -> applyFilterWithParallelism sharpen img parallelism 
            //         | Filters.sharpenSoft -> applyFilterWithParallelism sharpenSoft img parallelism 
            //         | Filters.sharpenWithEdges -> applyFilterWithParallelism sharpenWithEdges img parallelism 
            //         | Filters.emboss -> applyFilterWithParallelism emboss img parallelism 
            //         | Filters.embossHard -> applyFilterWithParallelism embossHard img parallelism 
            //         | _ -> 
            //             printfn "Unknown filter"
            //             img
            //     ) inImage
            
            // saveRgbaImageA resultImage outFolder |> ignore
            0 

open System
open System.Threading
open ImageProcessing.ImProcessing
open Argu


// Определяем типы сообщений
type Message =
    | Add of int * int * AsyncReplyChannel<int>
    | Subtract of int * int * AsyncReplyChannel<int>
    | Stop

// Создаем MailboxProcessor
let calculator = 
    MailboxProcessor<Message>.Start(fun inbox ->
        let rec loop () =
            async {
                let! msg = inbox.Receive() // Получаем сообщение из очереди
                match msg with
                | Add (x, y, replyChannel) ->
                    let result = x + y
                    replyChannel.Reply(result) // Отправляем ответ обратно
                    return! loop () // Продолжаем цикл

                | Subtract (x, y, replyChannel) ->
                    let result = x - y
                    replyChannel.Reply(result) // Отправляем ответ обратно
                    return! loop () // Продолжаем цикл

                | Stop ->
                    printfn "Stopping the calculator."
                    return () // Завершаем обработку
            }
        loop ()) // Запускаем цикл


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

    // Отправляем сообщения и ждем ответов
    let addResult = calculator.PostAndReply(fun replyChannel -> Add(5, 3, replyChannel))
    printfn "5 + 3 = %d" addResult

    let subtractResult = calculator.PostAndReply(fun replyChannel -> Subtract(10, 4, replyChannel))
    printfn "10 - 4 = %d" subtractResult

    // Останавливаем MailboxProcessor
    calculator.Post(Stop)
    
    0
    
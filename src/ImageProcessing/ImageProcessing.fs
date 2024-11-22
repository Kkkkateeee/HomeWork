module imageProcessing 

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing

[<Struct>]
type Image =
    val Data: array<byte>
    val Width: int
    val Height: int
    val Name: string

    new(data, width, height, name) =
        {
            Data = data
            Width = width
            Height = height
            Name = name
        }



let loadImageAsArray2D (filePath: string) =
    let image = Image.Load<L8>(filePath)

    let pixelArray2D = Array2D.zeroCreate image.Height image.Width

    for y in 0 .. image.Height - 1 do    
        for x in 0 .. image.Width - 1 do
            pixelArray2D[y, x] <- image[x, y]
    
    
let saveArray2DasImage (myArray2D: byte[,]) filePath =
    let heigh = myArray2D.GetLength 0
    let width = myArray2D.GetLength 1

    let array2Dto1D array2D =
        [|
            for x in [0 .. (Array2D.length1 array2D) - 1] do
                for y in [0 .. (Array2D.length2 array2D) - 1] do
                    yield array2D.[x, y]
        |]

    let image = Image.LoadPixelData<L8>(array2Dto1D myArray2D, width, heigh)
    image.Save filePath





let applyFilter1 (filter: float32[][]) (img: byte[,]) =
    let imgH = img.GetLength 0
    let imgW = img.GetLength 1

    let filterD = (Array.length filter) / 2

    let filter = Array.concat filter

    let processPixel px py =
        let dataToHandle = [|
            for i in px - filterD .. px + filterD do
                for j in py - filterD .. py + filterD do
                    if  i < 0
                        || i >= imgH
                        || j < 0
                        || j >= imgW
                    then float32 img.[px, py]
                    else float32 img.[i, j]
        |]

        Array.fold2 (fun s x y -> s + x * y) 0.0f filter dataToHandle

    Array2D.mapi (fun x y _ -> byte (processPixel x y)) img


    


let GaussianBlur = 
    [|
        [| 1;  4;  6;  4;  1 |]
        [| 4; 16; 24; 16;  4 |]
        [| 6; 24; 36; 24;  6 |]
        [| 4; 16; 24; 16;  4 |]
        [| 1;  4;  6;  4;  1 |]
    |]
    |> Array.map (Array.map (fun x -> (float32 x) / 256.0f))


    
(*
вопросы: 
1 структура RGBа - лучше сделать отдельную или как часть структуры Image - как часть структуры Image
4 на выходе appalyfilter должен быть 1мерный или 2мерный массив?  нужно применять фильтры друг за другом, чтобы не было преобразований 
*)


// [<Struct>]
// type RGBA =
//     val Red: int
//     val Green: int
//     val Blue: int
//     val Alpha: int
//     new(red, green, blue, alpha) = 
//         {
//             Red = red
//             Green = green
//             Blue = blue
//             Alpha = alpha
//         }


// let applyFilter (filter: float32[,]) (image: byte[,]) = 
    
//     let imageHeight = image.GetLength(0)
//     let imageWidth = image.GetLength(1)

//     let filterHeight = filter.GetLength(0)
//     let filterWidth = filter.GetLength(1)

//     let result: byte array = Array.zeroCreate<byte> (imageHeight * imageWidth)

//     for y in 0 .. imageHeight - 1 do
//         for x in 0 .. imageWidth - 1 do
//             let mutable intensity = 0.0

//             for fy in 0 .. filterHeight - 1 do    
//                 for fx in 0 .. filterWidth - 1 do

//                     let imageX = (x + fx - filterWidth / 2 + imageWidth) % imageWidth
//                     let imageY = (y + fy - filterHeight / 2 + imageHeight) % imageHeight

//                     intensity <- intensity + float(image[imageY, imageX]) * float(filter[fy, fx])
                
//             result[y * imageWidth + x] <- byte (min (max (int intensity) 0) 255)

//         result
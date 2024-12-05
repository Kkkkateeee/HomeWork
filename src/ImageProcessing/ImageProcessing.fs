namespace ImageProcessing

open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

[<Struct>]
type Image =
    val Data: array<Rgba32>
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
    static member Create(width: int, height: int) =
        let data = Array.init (height * width) (fun _ -> Rgba32(0uy, 0uy, 0uy, 255uy)) 
        Image(data, width, height, "New Image")


module ImProcessing =

    let gaussianBlur =
        [|
            [| 1;  4;  6;  4; 1 |]
            [| 4; 16; 24; 16; 4 |]
            [| 6; 24; 36; 24; 6 |]
            [| 4; 16; 24; 16; 4 |]
            [| 1;  4;  6;  4; 1 |]
        |]
        |> Array.map (Array.map (fun x -> (float32 x) / 256.0f))

    let motionDiagonal135deg =
        [|
            [| 1; 0; 0; 0; 0; 0; 0; 0; 0 |]
            [| 0; 1; 0; 0; 0; 0; 0; 0; 0 |]
            [| 0; 0; 1; 0; 0; 0; 0; 0; 0 |]
            [| 0; 0; 0; 1; 0; 0; 0; 0; 0 |]
            [| 0; 0; 0; 0; 1; 0; 0; 0; 0 |]
            [| 0; 0; 0; 0; 0; 1; 0; 0; 0 |]
            [| 0; 0; 0; 0; 0; 0; 1; 0; 0 |]
            [| 0; 0; 0; 0; 0; 0; 0; 1; 0 |]
            [| 0; 0; 0; 0; 0; 0; 0; 0; 1 |]
        |]
        |> Array.map (Array.map (fun x -> (float32 x) / 9.0f))

    let motionDiagonal315deg =
        [|
            [| 0; 0; 0; 0; 0; 0; 0; 0; 1 |]
            [| 0; 0; 0; 0; 0; 0; 0; 1; 0 |]
            [| 0; 0; 0; 0; 0; 0; 1; 0; 0 |]
            [| 0; 0; 0; 0; 0; 1; 0; 0; 0 |]
            [| 0; 0; 0; 0; 1; 0; 0; 0; 0 |]
            [| 0; 0; 0; 1; 0; 0; 0; 0; 0 |]
            [| 0; 0; 1; 0; 0; 0; 0; 0; 0 |]
            [| 0; 1; 0; 0; 0; 0; 0; 0; 0 |]
            [| 1; 0; 0; 0; 0; 0; 0; 0; 0 |]
        |]
        |> Array.map (Array.map (fun x -> (float32 x) / 9.0f))

    let motionVertical =
        [|
            [| 0; 0; 0; 0; 1; 0; 0; 0; 0 |]
            [| 0; 0; 0; 0; 1; 0; 0; 0; 0 |]
            [| 0; 0; 0; 0; 1; 0; 0; 0; 0 |]
            [| 0; 0; 0; 0; 1; 0; 0; 0; 0 |]
            [| 0; 0; 0; 0; 1; 0; 0; 0; 0 |]
            [| 0; 0; 0; 0; 1; 0; 0; 0; 0 |]
            [| 0; 0; 0; 0; 1; 0; 0; 0; 0 |]
            [| 0; 0; 0; 0; 1; 0; 0; 0; 0 |]
            [| 0; 0; 0; 0; 1; 0; 0; 0; 0 |]
        |]
        |> Array.map (Array.map (fun x -> (float32 x) / 9.0f))

    let motionHorizontal =
        [|
            [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
            [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
            [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
            [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
            [| 1; 1; 1; 1; 1; 1; 1; 1; 1 |]
            [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
            [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
            [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
            [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
        |]
        |> Array.map (Array.map (fun x -> (float32 x) / 9.0f))

    let edgesHorizontal =
        [|
            [| 0;  0; -1;  0;  0 |]
            [| 0;  0; -1;  0;  0 |]
            [| 0;  0;  2;  0;  0 |]
            [| 0;  0;  0;  0;  0 |]
            [| 0;  0;  0;  0;  0 |]
        |]
        |> Array.map (Array.map float32)

    let edgesVertical =
        [|
            [| 0;  0; -1;  0;  0 |]
            [| 0;  0; -1;  0;  0 |]
            [| 0;  0;  4;  0;  0 |]
            [| 0;  0; -1;  0;  0 |]
            [| 0;  0; -1;  0;  0 |]
        |]
        |> Array.map (Array.map float32)

    let edgesDioganal135deg =
        [|
            [| -1;  0;  0;  0;  0 |]
            [|  0; -2;  0;  0;  0 |]
            [|  0;  0;  6;  0;  0 |]
            [|  0;  0;  0; -2;  0 |]
            [|  0;  0;  0;  0; -1 |]
        |]
        |> Array.map (Array.map float32)
    
    let edgesDioganal315deg =
        [|
            [|  0;  0;  0;  0; -1 |]
            [|  0;  0;  0; -2;  0 |]
            [|  0;  0;  6;  0;  0 |]
            [|  0; -2;  0; -0;  0 |]
            [| -1;  0;  0;  0;  0 |]
        |]
        |> Array.map (Array.map float32)

    let edgesAllDirections =
        [|
            [| -1; -1; -1 |]
            [| -1;  8; -1 |]
            [| -1; -1; -1 |]
        |]
        |> Array.map (Array.map float32)

    let sharpen =
        [|
            [| -1; -1; -1 |]
            [| -1;  9; -1 |]
            [| -1; -1; -1 |]
        |]
        |> Array.map (Array.map float32)

    let sharpenSoft = 
        [|
            [| -1; -1; -1; -1; -1 |]
            [| -1;  2;  2;  2; -1 |]
            [| -1;  2;  8;  2; -1 |]
            [| -1;  2;  2;  2; -1 |]
            [| -1; -1; -1; -1; -1 |]
        |]
        |> Array.map (Array.map (fun x -> (float32 x) / 8.0f))

    let sharpenWithEdges =
        [|
            [| 1;  1;  1 |]
            [| 1; -7;  1 |]
            [| 1;  1;  1 |]
        |]
        |> Array.map (Array.map float32)

    let emboss = 
        [|
            [| -1; -1;  0 |]
            [| -1;  0;  1 |]
            [|  0;  1;  1 |]
        |]
        |> Array.map (Array.map (fun x -> (float32 x) / + 128.0f))

    let embossHard =
        [|
            [|-1; -1; -1; -1;  0 |]
            [|-1; -1; -1;  0;  1 |]
            [|-1; -1;  0;  1;  1 |]
            [|-1;  0;  1;  1;  1 |]
            [| 0;  1;  1;  1;  1 |]
        |]
        |> Array.map (Array.map (fun x -> (float32 x) / + 128.0f))


    let loadAsRgba2D (file: string) =
        let img = Image.Load<Rgba32> file
        let res = Array2D.zeroCreate<Rgba32> img.Height img.Width

        for i in 0 .. img.Width - 1 do
            for j in 0 .. img.Height - 1 do
                res.[j, i] <- img.[i, j]

        res

    let saveRgbaImage (rgbaData: Rgba32[,]) file =
        let h = rgbaData.GetLength(0)
        let w = rgbaData.GetLength(1)

        use img = new Image<Rgba32>(w, h)

        for x in 0 .. h - 1 do
            for y in 0 .. w - 1 do
                img.[y, x] <- rgbaData.[x, y] 

        img.Save(file)

    let applyFilter (filter: float32[][]) (img: Rgba32[,]) =
        let imgH = img.GetLength 0
        let imgW = img.GetLength 1

        let filterD = (Array.length filter) / 2
        let filter = Array.concat filter

        let processPixel px py =
            let dataToHandle = [| 
                for i in px - filterD .. px + filterD do
                    for j in py - filterD .. py + filterD do
                        if i < 0 || i >= imgH || j < 0 || j >= imgW then
                            (0.0f, 0.0f, 0.0f) 
                        else
                            let pixel = img.[i, j]
                            (float32 pixel.R, float32 pixel.G, float32 pixel.B)
            |]

            let mutable rSum = 0.0f
            let mutable gSum = 0.0f
            let mutable bSum = 0.0f

            Array.iteri (fun index (r, g, b) ->
                rSum <- rSum + r * filter.[index]
                gSum <- gSum + g * filter.[index]
                bSum <- bSum + b * filter.[index]
            ) dataToHandle

            (byte rSum, byte gSum, byte bSum)

        Array2D.mapi (fun x y _ ->
            let (r, g, b) = processPixel x y
            Rgba32(r, g, b, img.[x, y].A) 
        ) img
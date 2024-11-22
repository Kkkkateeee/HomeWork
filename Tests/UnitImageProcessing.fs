namespace UnitImageProcessing

open Xunit
open SixLabors.ImageSharp.PixelFormats

open ImageProcessing.IProcessing


module Data =
    
    let im1 = "../../../Images/image1.png"
    let imSmall = "../../../Images/image2x2px.png"
    let image1 = loadAsRgba2D im1
    let imageSmall = loadAsRgba2D imSmall

    let id = 
        [|  
            [| 0; 0; 0 |]
            [| 0; 1; 0 |]
            [| 0; 0; 0 |]
        |]
        |> Array.map (Array.map float32)

    let black =
        [|
            [| 0; 0; 0 |]
            [| 0; 0; 0 |]
            [| 0; 0; 0 |]
        |]
        |> Array.map (Array.map float32)

    let kernelTrivial1 =
        [|
            [| 0; 0; 0 |]
            [| 0; 0; 1 |]
            [| 0; 0; 0 |]
        |]
        |> Array.map (Array.map float32)

    let kernelTrivial2 =
        [|
            [| 0; 0; 0 |]
            [| 0; 0; 0 |]
            [| 0; 1; 0 |]
        |]
        |> Array.map (Array.map float32)

    let kernelTrivial12 =
        [|
            [| 0; 0; 0 |]
            [| 0; 0; 0 |]
            [| 0; 0; 1 |]
        |]
        |> Array.map (Array.map float32)

    let kernel =
        [|
            [| 1;  1;  1 |]
            [| 1; -7;  1 |]
            [| 1;  1;  1 |]
        |]
        |> Array.map (Array.map float32)

    let kernelExtended =
        [|
            [| 0;  0;  0;  0;  0 |]
            [| 0;  1;  1;  1;  0 |]
            [| 0;  1; -7;  1;  0 |]
            [| 0;  1;  1;  1;  0 |]
            [| 0;  0;  0;  0;  0 |]
        |]
        |> Array.map (Array.map float32)


    let imageIsBlack (image: Rgba32[,]) =
        let h = image.GetLength 0
        let w = image.GetLength 1

        let mutable isBlack = true

        for i in 0 .. h - 1 do
            for j in 0 .. w - 1 do
                let pixel = image.[i, j]
                if pixel <> Rgba32(byte 0, byte 0, byte 0, byte 255) then
                    isBlack <- false

        isBlack
    

module Filter =
    open Data

    [<Fact>]
    let filterDoesntChangeSize () =
        let imageAfterFilter = applyFilter gaussianBlur image1
        Assert.Equal(image1.GetLength(0), imageAfterFilter.GetLength(0))
        Assert.Equal(image1.GetLength(1), imageAfterFilter.GetLength(1))

    [<Fact>]
    let idDoesntChangeData () =
        let imageAfterFilter = applyFilter id image1
        Assert.Equal(image1, imageAfterFilter)

    [<Fact>]
    let imageSmallerThanFilter () =
        let imageAfterFilter = applyFilter black imageSmall
        Assert.Equal(true, imageIsBlack imageAfterFilter)

    [<Fact>]
    let blackFilter () =
        let imageBlack = applyFilter black image1
        Assert.Equal(true, imageIsBlack imageBlack)

    [<Fact>]
    let trivialComposition () =
        let trivial1and2 = applyFilter kernelTrivial1 image1 |> applyFilter kernelTrivial2
        let trivial12 = applyFilter kernelTrivial12 image1
        Assert.Equal(trivial1and2, trivial12)

    [<Fact>]
    let extendedComposition () =
        let imageAfterFilter = applyFilter kernel image1 
        let imageAfterExtendedFilter = applyFilter kernelExtended image1
        Assert.Equal(imageAfterFilter, imageAfterExtendedFilter)

    [<Fact>]
    let someAreCommutative () =
        let image12 = applyFilter kernelTrivial1 image1 |> applyFilter kernelTrivial2
        let image21 = applyFilter kernelTrivial2 image1 |> applyFilter kernelTrivial1
        Assert.Equal(image12, image21)
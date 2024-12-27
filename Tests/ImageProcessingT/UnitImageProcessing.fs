namespace UnitImageProcessing

open Xunit
open SixLabors.ImageSharp.PixelFormats

open ImageProcessing.ImProcessing


module Data =
    
    let im1 = "../../../Images/image1.png"
    let imSmall = "../../../Images/image2x2px.png"
    let image1 = loadAsRgba2D im1
    let imageSmall = loadAsRgba2D imSmall

    let imId = "../../../Images/id.png"
    let imShiftRightDown = "../../../Images/shiftRightDown.png"
    let imShiftDownRight = "../../../Images/shiftDownRight.png"
    let imShiftDiagonal = "../../../Images/shiftDiagonal.png"
    let imKernel = "../../../Images/kernel.png"
    let imKernelExtended = "../../../Images/kernelExtended.png"
    let imageId= loadAsRgba2D imId
    let imageShiftRightDown= loadAsRgba2D imShiftRightDown
    let imageShiftDownRight= loadAsRgba2D imShiftDownRight
    let imageShiftDiagonal= loadAsRgba2D imShiftDiagonal
    let imageKernel= loadAsRgba2D imKernel
    let imageKernelExtended= loadAsRgba2D imKernelExtended


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

    let shiftRight =
        [|
            [| 0; 0; 0 |]
            [| 0; 0; 1 |]
            [| 0; 0; 0 |]
        |]
        |> Array.map (Array.map float32)

    let shiftDown =
        [|
            [| 0; 0; 0 |]
            [| 0; 0; 0 |]
            [| 0; 1; 0 |]
        |]
        |> Array.map (Array.map float32)

    let shiftDiagonal =
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
    let idDoesntChangeData () =
        let imageAfterFilter = applyFilter id image1
        Assert.Equal(imageId, imageAfterFilter)

    [<Fact>]
    let shiftComposition () =
        let shiftRightDown = applyFilter shiftRight image1 |> applyFilter shiftDown
        let shiftDiagonal = applyFilter shiftDiagonal image1
        Assert.Equal(imageShiftRightDown, imageShiftDiagonal)
        Assert.Equal(imageShiftRightDown, shiftRightDown)
        Assert.Equal(imageShiftRightDown, shiftDiagonal)

    [<Fact>]
    let extendedComposition () =
        let kernel = applyFilter kernel image1 
        let kernelExtended = applyFilter kernelExtended image1
        Assert.Equal(imageKernel, imageKernelExtended)
        Assert.Equal(imageKernel, kernel)
        Assert.Equal(imageKernel, kernelExtended)

    [<Fact>]
    let someAreCommutative () =
        let imageRD = applyFilter shiftRight image1 |> applyFilter shiftDown
        let imageDR = applyFilter shiftDown image1 |> applyFilter shiftRight
        Assert.Equal(imageShiftDownRight, imageShiftRightDown)
        Assert.Equal(imageShiftDownRight, imageDR)
        Assert.Equal(imageShiftDownRight, imageRD)
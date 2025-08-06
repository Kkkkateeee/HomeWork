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
        let resNoParallelism = applyFilterNoParallelism id image1
        let resPixelParallelism = applyFilterPixelParallelism id image1
        let resPartsParallelism = applyFilterParallelismInParts id image1
        let resRowParallelism = applyFilterRowParallelism id image1
        let resColParallelism = applyFilterColParallelism id image1
        Assert.Equal(imageId, resNoParallelism)
        Assert.Equal(imageId, resPixelParallelism)
        Assert.Equal(imageId, resPartsParallelism)
        Assert.Equal(imageId, resRowParallelism)
        Assert.Equal(imageId, resColParallelism)

    [<Fact>]
    let shiftComposition () =
        let shiftRightDown_NoParallelism = applyFilterNoParallelism shiftRight image1 |> applyFilterNoParallelism shiftDown
        let shiftDiagonal_NoParallelism = applyFilterNoParallelism shiftDiagonal image1

        let shiftRightDown_PixelParallelism = applyFilterPixelParallelism shiftRight image1 |> applyFilterPixelParallelism shiftDown
        let shiftDiagonal_PixelParallelism = applyFilterPixelParallelism shiftDiagonal image1

        let shiftRightDown_PartsParallelism = applyFilterParallelismInParts shiftRight image1 |> applyFilterParallelismInParts shiftDown
        let shiftDiagonal_PartsParallelism = applyFilterParallelismInParts shiftDiagonal image1

        let shiftRightDown_RowParallelism = applyFilterRowParallelism shiftRight image1 |> applyFilterRowParallelism shiftDown
        let shiftDiagonal_RowParallelism = applyFilterRowParallelism shiftDiagonal image1

        let shiftRightDown_ColParallelism = applyFilterColParallelism shiftRight image1 |> applyFilterColParallelism shiftDown
        let shiftDiagonal_ColParallelism = applyFilterColParallelism shiftDiagonal image1

        Assert.Equal(imageShiftRightDown, imageShiftDiagonal)
        Assert.Equal(imageShiftRightDown, shiftRightDown_NoParallelism)
        Assert.Equal(imageShiftRightDown, shiftDiagonal_NoParallelism)

        Assert.Equal(imageShiftRightDown, shiftRightDown_PixelParallelism)
        Assert.Equal(imageShiftRightDown, shiftDiagonal_PixelParallelism)

        Assert.Equal(imageShiftRightDown, shiftRightDown_PartsParallelism)
        Assert.Equal(imageShiftRightDown, shiftDiagonal_PartsParallelism)

        Assert.Equal(imageShiftRightDown, shiftRightDown_RowParallelism)
        Assert.Equal(imageShiftRightDown, shiftDiagonal_RowParallelism)

        Assert.Equal(imageShiftRightDown, shiftRightDown_ColParallelism)
        Assert.Equal(imageShiftRightDown, shiftDiagonal_ColParallelism)

    [<Fact>]
    let extendedComposition () =
        let kernel_NoParallelism = applyFilterNoParallelism kernel image1 
        let kernelExtended_NoParallelism = applyFilterNoParallelism kernelExtended image1

        let kernel_PixelParallelism = applyFilterPixelParallelism kernel image1 
        let kernelExtended_PixelParallelism = applyFilterPixelParallelism kernelExtended image1

        let kernel_PartsParallelism = applyFilterParallelismInParts kernel image1 
        let kernelExtended_PartsParallelism = applyFilterParallelismInParts kernelExtended image1

        let kernel_RowParallelism = applyFilterRowParallelism kernel image1 
        let kernelExtended_RowParallelism = applyFilterRowParallelism kernelExtended image1

        let kernel_ColParallelism = applyFilterColParallelism kernel image1 
        let kernelExtended_ColParallelism = applyFilterColParallelism kernelExtended image1

        Assert.Equal(imageKernel, imageKernelExtended)
        Assert.Equal(imageKernel, kernel_NoParallelism)
        Assert.Equal(imageKernel, kernelExtended_NoParallelism)

        Assert.Equal(imageKernel, kernel_PixelParallelism)
        Assert.Equal(imageKernel, kernelExtended_PixelParallelism)

        Assert.Equal(imageKernel, kernel_PartsParallelism)
        Assert.Equal(imageKernel, kernelExtended_PartsParallelism)

        Assert.Equal(imageKernel, kernel_RowParallelism)
        Assert.Equal(imageKernel, kernelExtended_RowParallelism)

        Assert.Equal(imageKernel, kernel_ColParallelism)
        Assert.Equal(imageKernel, kernelExtended_ColParallelism)

    [<Fact>]
    let someAreCommutative () =
        let imageRD_NoParallelism = applyFilterNoParallelism shiftRight image1 |> applyFilterNoParallelism shiftDown
        let imageDR_NoParallelism = applyFilterNoParallelism shiftDown image1 |> applyFilterNoParallelism shiftRight

        let imageRD_PixelParallelism = applyFilterPixelParallelism shiftRight image1 |> applyFilterPixelParallelism shiftDown
        let imageDR_PixelParallelism = applyFilterPixelParallelism shiftDown image1 |> applyFilterPixelParallelism shiftRight

        let imageRD_PartsParallelism = applyFilterParallelismInParts shiftRight image1 |> applyFilterParallelismInParts shiftDown
        let imageDR_PartsParallelism = applyFilterParallelismInParts shiftDown image1 |> applyFilterParallelismInParts shiftRight

        let imageRD_RowParallelism = applyFilterRowParallelism shiftRight image1 |> applyFilterRowParallelism shiftDown
        let imageDR_RowParallelism = applyFilterRowParallelism shiftDown image1 |> applyFilterRowParallelism shiftRight

        let imageRD_ColParallelism = applyFilterColParallelism shiftRight image1 |> applyFilterColParallelism shiftDown
        let imageDR_ColParallelism = applyFilterColParallelism shiftDown image1 |> applyFilterColParallelism shiftRight

        Assert.Equal(imageShiftDownRight, imageShiftRightDown)
        Assert.Equal(imageShiftDownRight, imageDR_NoParallelism)
        Assert.Equal(imageShiftDownRight, imageRD_NoParallelism)

        Assert.Equal(imageShiftDownRight, imageDR_PixelParallelism)
        Assert.Equal(imageShiftDownRight, imageRD_PixelParallelism)

        Assert.Equal(imageShiftDownRight, imageDR_PartsParallelism)
        Assert.Equal(imageShiftDownRight, imageRD_PartsParallelism)

        Assert.Equal(imageShiftDownRight, imageDR_RowParallelism)
        Assert.Equal(imageShiftDownRight, imageRD_RowParallelism)

        Assert.Equal(imageShiftDownRight, imageDR_ColParallelism)
        Assert.Equal(imageShiftDownRight, imageRD_ColParallelism)
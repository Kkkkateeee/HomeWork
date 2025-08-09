namespace UnitImageProcessing

open Xunit
open SixLabors.ImageSharp.PixelFormats

open ImageProcessing.ImProcessing


module Data =
    
    let im1 = "../../../Images/image1.png"
    let imSmall = "../../../Images/image2x2px.png"
    let image1 = loadAsRgba2DA im1
    let imageSmall = loadAsRgba2DA imSmall

    let imId = "../../../Images/id.png"
    let imShiftRightDown = "../../../Images/shiftRightDown.png"
    let imShiftDownRight = "../../../Images/shiftDownRight.png"
    let imShiftDiagonal = "../../../Images/shiftDiagonal.png"
    let imKernel = "../../../Images/kernel.png"
    let imKernelExtended = "../../../Images/kernelExtended.png"
    let imageId= loadAsRgba2DA imId |> Async.RunSynchronously
    let imageShiftRightDown= loadAsRgba2DA imShiftRightDown |> Async.RunSynchronously
    let imageShiftDownRight= loadAsRgba2DA imShiftDownRight |> Async.RunSynchronously
    let imageShiftDiagonal= loadAsRgba2DA imShiftDiagonal |> Async.RunSynchronously
    let imageKernel= loadAsRgba2DA imKernel |> Async.RunSynchronously
    let imageKernelExtended= loadAsRgba2DA imKernelExtended |> Async.RunSynchronously


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


    let imageIsBlack (imageAsync: Async<Rgba32[,]>) =
        async {
            let! image = imageAsync
            let h = image.GetLength 0
            let w = image.GetLength 1

            let mutable isBlack = true

            for i in 0 .. h - 1 do
                for j in 0 .. w - 1 do
                    let pixel = image.[i, j]
                    if pixel <> Rgba32(byte 0, byte 0, byte 0, byte 255) then
                        isBlack <- false

            return isBlack
        } |> Async.RunSynchronously

    

module Filter =
    open Data

    [<Fact>]
    let idDoesntChangeData () =
        let resNoParallelism = applyFilterNoParallelismA id image1
        let resPixelParallelism = applyFilterPixelParallelismA id image1
        let resPartsParallelism = applyFilterParallelismInPartsA id image1
        let resRowParallelism = applyFilterRowParallelismA id image1
        let resColParallelism = applyFilterColParallelismA id image1

        Assert.Equal(imageId, resNoParallelism |> Async.RunSynchronously)
        Assert.Equal(imageId, resPixelParallelism |> Async.RunSynchronously)
        Assert.Equal(imageId, resPartsParallelism |> Async.RunSynchronously)
        Assert.Equal(imageId, resRowParallelism |> Async.RunSynchronously)
        Assert.Equal(imageId, resColParallelism |> Async.RunSynchronously)

    [<Fact>]
    let shiftComposition () =
        let shiftRightDown_NoParallelism = applyFilterNoParallelismA shiftRight image1 |> applyFilterNoParallelismA shiftDown
        let shiftDiagonal_NoParallelism = applyFilterNoParallelismA shiftDiagonal image1

        let shiftRightDown_PixelParallelism = applyFilterPixelParallelismA shiftRight image1 |> applyFilterPixelParallelismA shiftDown
        let shiftDiagonal_PixelParallelism = applyFilterPixelParallelismA shiftDiagonal image1

        let shiftRightDown_PartsParallelism = applyFilterParallelismInPartsA shiftRight image1 |> applyFilterParallelismInPartsA shiftDown
        let shiftDiagonal_PartsParallelism = applyFilterParallelismInPartsA shiftDiagonal image1

        let shiftRightDown_RowParallelism = applyFilterRowParallelismA shiftRight image1 |> applyFilterRowParallelismA shiftDown
        let shiftDiagonal_RowParallelism = applyFilterRowParallelismA shiftDiagonal image1

        let shiftRightDown_ColParallelism = applyFilterColParallelismA shiftRight image1 |> applyFilterColParallelismA shiftDown
        let shiftDiagonal_ColParallelism = applyFilterColParallelismA shiftDiagonal image1

        Assert.Equal(imageShiftRightDown, imageShiftDiagonal)
        Assert.Equal(imageShiftRightDown, shiftRightDown_NoParallelism |> Async.RunSynchronously)
        Assert.Equal(imageShiftRightDown, shiftDiagonal_NoParallelism |> Async.RunSynchronously)

        Assert.Equal(imageShiftRightDown, shiftRightDown_PixelParallelism |> Async.RunSynchronously)
        Assert.Equal(imageShiftRightDown, shiftDiagonal_PixelParallelism |> Async.RunSynchronously)

        Assert.Equal(imageShiftRightDown, shiftRightDown_PartsParallelism |> Async.RunSynchronously)
        Assert.Equal(imageShiftRightDown, shiftDiagonal_PartsParallelism |> Async.RunSynchronously)

        Assert.Equal(imageShiftRightDown, shiftRightDown_RowParallelism |> Async.RunSynchronously)
        Assert.Equal(imageShiftRightDown, shiftDiagonal_RowParallelism |> Async.RunSynchronously)

        Assert.Equal(imageShiftRightDown, shiftRightDown_ColParallelism |> Async.RunSynchronously)
        Assert.Equal(imageShiftRightDown, shiftDiagonal_ColParallelism |> Async.RunSynchronously)

    [<Fact>]
    let extendedComposition () =
        let kernel_NoParallelism = applyFilterNoParallelismA kernel image1 
        let kernelExtended_NoParallelism = applyFilterNoParallelismA kernelExtended image1

        let kernel_PixelParallelism = applyFilterPixelParallelismA kernel image1 
        let kernelExtended_PixelParallelism = applyFilterPixelParallelismA kernelExtended image1

        let kernel_PartsParallelism = applyFilterParallelismInPartsA kernel image1 
        let kernelExtended_PartsParallelism = applyFilterParallelismInPartsA kernelExtended image1

        let kernel_RowParallelism = applyFilterRowParallelismA kernel image1 
        let kernelExtended_RowParallelism = applyFilterRowParallelismA kernelExtended image1

        let kernel_ColParallelism = applyFilterColParallelismA kernel image1 
        let kernelExtended_ColParallelism = applyFilterColParallelismA kernelExtended image1

        Assert.Equal(imageKernel, imageKernelExtended)
        Assert.Equal(imageKernel, kernel_NoParallelism |> Async.RunSynchronously)
        Assert.Equal(imageKernel, kernelExtended_NoParallelism |> Async.RunSynchronously)

        Assert.Equal(imageKernel, kernel_PixelParallelism |> Async.RunSynchronously)
        Assert.Equal(imageKernel, kernelExtended_PixelParallelism |> Async.RunSynchronously)

        Assert.Equal(imageKernel, kernel_PartsParallelism |> Async.RunSynchronously)
        Assert.Equal(imageKernel, kernelExtended_PartsParallelism |> Async.RunSynchronously)

        Assert.Equal(imageKernel, kernel_RowParallelism |> Async.RunSynchronously)
        Assert.Equal(imageKernel, kernelExtended_RowParallelism |> Async.RunSynchronously)

        Assert.Equal(imageKernel, kernel_ColParallelism |> Async.RunSynchronously)
        Assert.Equal(imageKernel, kernelExtended_ColParallelism |> Async.RunSynchronously)

    [<Fact>]
    let someAreCommutative () =
        let imageRD_NoParallelism = applyFilterNoParallelismA shiftRight image1 |> applyFilterNoParallelismA shiftDown
        let imageDR_NoParallelism = applyFilterNoParallelismA shiftDown image1 |> applyFilterNoParallelismA shiftRight

        let imageRD_PixelParallelism = applyFilterPixelParallelismA shiftRight image1 |> applyFilterPixelParallelismA shiftDown
        let imageDR_PixelParallelism = applyFilterPixelParallelismA shiftDown image1 |> applyFilterPixelParallelismA shiftRight

        let imageRD_PartsParallelism = applyFilterParallelismInPartsA shiftRight image1 |> applyFilterParallelismInPartsA shiftDown
        let imageDR_PartsParallelism = applyFilterParallelismInPartsA shiftDown image1 |> applyFilterParallelismInPartsA shiftRight

        let imageRD_RowParallelism = applyFilterRowParallelismA shiftRight image1 |> applyFilterRowParallelismA shiftDown
        let imageDR_RowParallelism = applyFilterRowParallelismA shiftDown image1 |> applyFilterRowParallelismA shiftRight

        let imageRD_ColParallelism = applyFilterColParallelismA shiftRight image1 |> applyFilterColParallelismA shiftDown
        let imageDR_ColParallelism = applyFilterColParallelismA shiftDown image1 |> applyFilterColParallelismA shiftRight

        Assert.Equal(imageShiftDownRight, imageShiftRightDown)
        Assert.Equal(imageShiftDownRight, imageDR_NoParallelism |> Async.RunSynchronously)
        Assert.Equal(imageShiftDownRight, imageRD_NoParallelism |> Async.RunSynchronously)

        Assert.Equal(imageShiftDownRight, imageDR_PixelParallelism |> Async.RunSynchronously)
        Assert.Equal(imageShiftDownRight, imageRD_PixelParallelism |> Async.RunSynchronously)

        Assert.Equal(imageShiftDownRight, imageDR_PartsParallelism |> Async.RunSynchronously)
        Assert.Equal(imageShiftDownRight, imageRD_PartsParallelism |> Async.RunSynchronously)

        Assert.Equal(imageShiftDownRight, imageDR_RowParallelism |> Async.RunSynchronously)
        Assert.Equal(imageShiftDownRight, imageRD_RowParallelism |> Async.RunSynchronously)

        Assert.Equal(imageShiftDownRight, imageDR_ColParallelism |> Async.RunSynchronously)
        Assert.Equal(imageShiftDownRight, imageRD_ColParallelism |> Async.RunSynchronously)
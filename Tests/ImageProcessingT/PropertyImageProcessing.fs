namespace PropertyImageProcessing

open Xunit
open FsCheck
open FsCheck.Xunit
open SixLabors.ImageSharp.PixelFormats

open ImageProcessing.ImProcessing
open UnitImageProcessing.Data


module ImageGen =
    let private pixelGen : Gen<Rgba32> =
        gen {
            let! r = Gen.choose(0, 255) |> Gen.map byte
            let! g = Gen.choose(0, 255) |> Gen.map byte
            let! b = Gen.choose(0, 255) |> Gen.map byte
            let a = byte 255
            return Rgba32(r, g, b, a)
        }
    let array2DGen (size: int) : Gen<Rgba32[,]> =
        gen {
            let! pixels = Gen.array2DOfDim (size, size) pixelGen
            return pixels
        }
    
type Image100() =
    static member Rgba32() =
        Arb.fromGen (ImageGen.array2DGen 100)

type Image2() =
    static member Rgba32() =
        Arb.fromGen (ImageGen.array2DGen 2)


[<Properties(MaxTest = 100)>]
type Filter() =

    [<Property(Arbitrary = [| typeof<Image100> |])>]
    member _.filterDoesntChangeSize (image: Rgba32[,]) =
        let resNoParallelism = applyFilterNoParallelism gaussianBlur image
        let resPixelParallelism = applyFilterPixelParallelism gaussianBlur image
        let resPartsParallelism = applyFilterParallelismInParts gaussianBlur image
        let resRowParallelism = applyFilterRowParallelism gaussianBlur image
        let resColParallelism = applyFilterColParallelism gaussianBlur image

        Assert.Equal(100, resNoParallelism.GetLength 0)
        Assert.Equal(100, resNoParallelism.GetLength 1)

        Assert.Equal(100, resPixelParallelism.GetLength 0)
        Assert.Equal(100, resPixelParallelism.GetLength 1)

        Assert.Equal(100, resPartsParallelism.GetLength 0)
        Assert.Equal(100, resPartsParallelism.GetLength 1)

        Assert.Equal(100, resRowParallelism.GetLength 0)
        Assert.Equal(100, resRowParallelism.GetLength 1)

        Assert.Equal(100, resColParallelism.GetLength 0)
        Assert.Equal(100, resColParallelism.GetLength 1)

    [<Property(Arbitrary = [| typeof<Image100> |])>]
    member _.idDoesntChangeData (image: Rgba32[,]) =
        let resNoParallelism = applyFilterNoParallelism id image
        let resPixelParallelism = applyFilterPixelParallelism id image
        let resPartsParallelism = applyFilterParallelismInParts id image
        let resRowParallelism = applyFilterRowParallelism id image
        let resColParallelism = applyFilterColParallelism id image

        Assert.Equal(image, resNoParallelism)
        Assert.Equal(image, resPixelParallelism)
        Assert.Equal(image, resPartsParallelism)
        Assert.Equal(image, resRowParallelism)
        Assert.Equal(image, resColParallelism)

    [<Property(Arbitrary = [| typeof<Image2> |])>]
    member _.imageSmallerThanFilter (image: Rgba32[,]) =
        let resNoParallelism = applyFilterNoParallelism black image
        let resPixelParallelism = applyFilterPixelParallelism black image
        let resPartsParallelism = applyFilterParallelismInParts black image
        let resRowParallelism = applyFilterRowParallelism black image
        let resColParallelism = applyFilterColParallelism black image
        
        Assert.Equal(true, imageIsBlack resNoParallelism)
        Assert.Equal(true, imageIsBlack resPixelParallelism)
        Assert.Equal(true, imageIsBlack resPartsParallelism)
        Assert.Equal(true, imageIsBlack resRowParallelism)
        Assert.Equal(true, imageIsBlack resColParallelism)

    [<Property(Arbitrary = [| typeof<Image100> |])>]
    member _.blackFilter (image: Rgba32[,]) =
        let imageBlack_NoParallelism = applyFilterNoParallelism black image
        let imageBlack_PixelParallelism = applyFilterPixelParallelism black image
        let imageBlack_PartsParallelism = applyFilterParallelismInParts black image
        let imageBlac_RowParallelism = applyFilterRowParallelism black image
        let imageBlack_ColParallelism = applyFilterColParallelism black image
        
        Assert.Equal(true, imageIsBlack imageBlack_NoParallelism)
        Assert.Equal(true, imageIsBlack imageBlack_PixelParallelism)
        Assert.Equal(true, imageIsBlack imageBlack_PartsParallelism)
        Assert.Equal(true, imageIsBlack imageBlac_RowParallelism)
        Assert.Equal(true, imageIsBlack imageBlack_ColParallelism)

    [<Property(Arbitrary = [| typeof<Image100> |])>]
    member _.shiftComposition (image: Rgba32[,]) =
        let trivial1and2_NoParallelism = applyFilterNoParallelism shiftRight image |> applyFilterNoParallelism shiftDown
        let trivial12_NoParallelism = applyFilterNoParallelism shiftDiagonal image

        let trivial1and2_PixelParallelism = applyFilterPixelParallelism shiftRight image |> applyFilterPixelParallelism shiftDown
        let trivial12_PixelParallelism = applyFilterPixelParallelism shiftDiagonal image

        let trivial1and2_PartsParallelism = applyFilterParallelismInParts shiftRight image |> applyFilterParallelismInParts shiftDown
        let trivial12_PartsParallelism = applyFilterParallelismInParts shiftDiagonal image

        let trivial1and2_RowParallelism = applyFilterRowParallelism shiftRight image |> applyFilterRowParallelism shiftDown
        let trivial12_RowParallelism = applyFilterRowParallelism shiftDiagonal image

        let trivial1and2_ColParallelism = applyFilterColParallelism shiftRight image |> applyFilterColParallelism shiftDown
        let trivial12_ColParallelism = applyFilterColParallelism shiftDiagonal image

        Assert.Equal(trivial1and2_NoParallelism, trivial12_NoParallelism)
        Assert.Equal(trivial1and2_PixelParallelism, trivial12_PixelParallelism)
        Assert.Equal(trivial1and2_PartsParallelism, trivial12_PartsParallelism)
        Assert.Equal(trivial1and2_RowParallelism, trivial12_RowParallelism)
        Assert.Equal(trivial1and2_ColParallelism, trivial12_ColParallelism)

    [<Property(Arbitrary = [| typeof<Image100> |])>]
    member _.extendedComposition (image: Rgba32[,])  =
        let imageAfterFilter_NoParallelism = applyFilterNoParallelism kernel image
        let imageAfterExtendedFilter_NoParallelism = applyFilterNoParallelism kernelExtended image

        let imageAfterFilter_PixelParallelism = applyFilterPixelParallelism kernel image
        let imageAfterExtendedFilter_PixelParallelism = applyFilterPixelParallelism kernelExtended image

        let imageAfterFilter_PartsParallelism = applyFilterParallelismInParts kernel image
        let imageAfterExtendedFilter_PartsParallelism = applyFilterParallelismInParts kernelExtended image

        let imageAfterFilter_RowParallelism = applyFilterRowParallelism kernel image
        let imageAfterExtendedFilter_RowParallelism = applyFilterRowParallelism kernelExtended image

        let imageAfterFilter_ColParallelism = applyFilterColParallelism kernel image
        let imageAfterExtendedFilter_ColParallelism = applyFilterColParallelism kernelExtended image

        Assert.Equal(imageAfterFilter_NoParallelism, imageAfterExtendedFilter_NoParallelism)
        Assert.Equal(imageAfterFilter_PixelParallelism, imageAfterExtendedFilter_PixelParallelism)
        Assert.Equal(imageAfterFilter_PartsParallelism, imageAfterExtendedFilter_PartsParallelism)
        Assert.Equal(imageAfterFilter_RowParallelism, imageAfterExtendedFilter_RowParallelism)
        Assert.Equal(imageAfterFilter_ColParallelism, imageAfterExtendedFilter_ColParallelism)

    [<Property(Arbitrary = [| typeof<Image100> |])>]
    member _.someAreCommutative (image: Rgba32[,]) =
        let image12_NoParallelism = applyFilterNoParallelism shiftRight image |> applyFilterNoParallelism shiftDown
        let image21_NoParallelism = applyFilterNoParallelism shiftDown image |> applyFilterNoParallelism shiftRight

        let image12_PixelParallelism = applyFilterPixelParallelism shiftRight image |> applyFilterPixelParallelism shiftDown
        let image21_PixelParallelism = applyFilterPixelParallelism shiftDown image |> applyFilterPixelParallelism shiftRight

        let image12_PartsParallelism = applyFilterParallelismInParts shiftRight image |> applyFilterParallelismInParts shiftDown
        let image21_PartsParallelism = applyFilterParallelismInParts shiftDown image |> applyFilterParallelismInParts shiftRight

        let image12_RowParallelism = applyFilterRowParallelism shiftRight image |> applyFilterRowParallelism shiftDown
        let image21_RowParallelism = applyFilterRowParallelism shiftDown image |> applyFilterRowParallelism shiftRight

        let image12_ColParallelism = applyFilterColParallelism shiftRight image |> applyFilterColParallelism shiftDown
        let image21_ColParallelism = applyFilterColParallelism shiftDown image |> applyFilterColParallelism shiftRight
        
        Assert.Equal(image12_NoParallelism, image21_NoParallelism)
        Assert.Equal(image12_PixelParallelism, image21_PixelParallelism)
        Assert.Equal(image12_PartsParallelism, image21_PartsParallelism)
        Assert.Equal(image12_RowParallelism, image21_RowParallelism)
        Assert.Equal(image12_ColParallelism, image21_ColParallelism)
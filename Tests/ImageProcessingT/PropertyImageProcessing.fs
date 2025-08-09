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
    let array2DGen (size: int) : Gen<Async<Rgba32[, ]>> = 
        gen {
            let! pixels = Gen.array2DOfDim (size, size) pixelGen
            return async { return pixels }
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
    member _.filterDoesntChangeSize (image: Async<Rgba32[, ]>) =
        let resNoParallelism = 
            applyFilterNoParallelismA gaussianBlur image
            |> Async.RunSynchronously

        let resPixelParallelism = 
            applyFilterPixelParallelismA gaussianBlur image
            |> Async.RunSynchronously

        let resPartsParallelism = 
            applyFilterParallelismInPartsA gaussianBlur image
            |> Async.RunSynchronously

        let resRowParallelism = 
            applyFilterRowParallelismA gaussianBlur image
            |> Async.RunSynchronously

        let resColParallelism = 
            applyFilterColParallelismA gaussianBlur image
            |> Async.RunSynchronously

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
    member _.idDoesntChangeData (image: Async<Rgba32[, ]>) =
        let resNoParallelism = applyFilterNoParallelismA id image
        let resPixelParallelism = applyFilterPixelParallelismA id image
        let resPartsParallelism = applyFilterParallelismInPartsA id image
        let resRowParallelism = applyFilterRowParallelismA id image
        let resColParallelism = applyFilterColParallelismA id image

        Assert.Equal(image, resNoParallelism)
        Assert.Equal(image, resPixelParallelism)
        Assert.Equal(image, resPartsParallelism)
        Assert.Equal(image, resRowParallelism)
        Assert.Equal(image, resColParallelism)

    [<Property(Arbitrary = [| typeof<Image2> |])>]
    member _.imageSmallerThanFilter (image: Async<Rgba32[, ]>) =
        let resNoParallelism = applyFilterNoParallelismA black image
        let resPixelParallelism = applyFilterPixelParallelismA black image
        let resPartsParallelism = applyFilterParallelismInPartsA black image
        let resRowParallelism = applyFilterRowParallelismA black image
        let resColParallelism = applyFilterColParallelismA black image
        
        Assert.Equal(true, imageIsBlack resNoParallelism)
        Assert.Equal(true, imageIsBlack resPixelParallelism)
        Assert.Equal(true, imageIsBlack resPartsParallelism)
        Assert.Equal(true, imageIsBlack resRowParallelism)
        Assert.Equal(true, imageIsBlack resColParallelism)

    [<Property(Arbitrary = [| typeof<Image100> |])>]
    member _.blackFilter (image: Async<Rgba32[, ]>) =
        let imageBlack_NoParallelism = applyFilterNoParallelismA black image
        let imageBlack_PixelParallelism = applyFilterPixelParallelismA black image
        let imageBlack_PartsParallelism = applyFilterParallelismInPartsA black image
        let imageBlac_RowParallelism = applyFilterRowParallelismA black image
        let imageBlack_ColParallelism = applyFilterColParallelismA black image
        
        Assert.Equal(true, imageIsBlack imageBlack_NoParallelism)
        Assert.Equal(true, imageIsBlack imageBlack_PixelParallelism)
        Assert.Equal(true, imageIsBlack imageBlack_PartsParallelism)
        Assert.Equal(true, imageIsBlack imageBlac_RowParallelism)
        Assert.Equal(true, imageIsBlack imageBlack_ColParallelism)

    [<Property(Arbitrary = [| typeof<Image100> |])>]
    member _.shiftComposition (image: Async<Rgba32[, ]>) =
        let trivial1and2_NoParallelism = applyFilterNoParallelismA shiftRight image |> applyFilterNoParallelismA shiftDown
        let trivial12_NoParallelism = applyFilterNoParallelismA shiftDiagonal image

        let trivial1and2_PixelParallelism = applyFilterPixelParallelismA shiftRight image |> applyFilterPixelParallelismA shiftDown
        let trivial12_PixelParallelism = applyFilterPixelParallelismA shiftDiagonal image

        let trivial1and2_PartsParallelism = applyFilterParallelismInPartsA shiftRight image |> applyFilterParallelismInPartsA shiftDown
        let trivial12_PartsParallelism = applyFilterParallelismInPartsA shiftDiagonal image

        let trivial1and2_RowParallelism = applyFilterRowParallelismA shiftRight image |> applyFilterRowParallelismA shiftDown
        let trivial12_RowParallelism = applyFilterRowParallelismA shiftDiagonal image

        let trivial1and2_ColParallelism = applyFilterColParallelismA shiftRight image |> applyFilterColParallelismA shiftDown
        let trivial12_ColParallelism = applyFilterColParallelismA shiftDiagonal image

        Assert.Equal(trivial1and2_NoParallelism, trivial12_NoParallelism)
        Assert.Equal(trivial1and2_PixelParallelism, trivial12_PixelParallelism)
        Assert.Equal(trivial1and2_PartsParallelism, trivial12_PartsParallelism)
        Assert.Equal(trivial1and2_RowParallelism, trivial12_RowParallelism)
        Assert.Equal(trivial1and2_ColParallelism, trivial12_ColParallelism)

    [<Property(Arbitrary = [| typeof<Image100> |])>]
    member _.extendedComposition (image: Async<Rgba32[, ]>)  =
        let imageAfterFilter_NoParallelism = applyFilterNoParallelismA kernel image
        let imageAfterExtendedFilter_NoParallelism = applyFilterNoParallelismA kernelExtended image

        let imageAfterFilter_PixelParallelism = applyFilterPixelParallelismA kernel image
        let imageAfterExtendedFilter_PixelParallelism = applyFilterPixelParallelismA kernelExtended image

        let imageAfterFilter_PartsParallelism = applyFilterParallelismInPartsA kernel image
        let imageAfterExtendedFilter_PartsParallelism = applyFilterParallelismInPartsA kernelExtended image

        let imageAfterFilter_RowParallelism = applyFilterRowParallelismA kernel image
        let imageAfterExtendedFilter_RowParallelism = applyFilterRowParallelismA kernelExtended image

        let imageAfterFilter_ColParallelism = applyFilterColParallelismA kernel image
        let imageAfterExtendedFilter_ColParallelism = applyFilterColParallelismA kernelExtended image

        Assert.Equal(imageAfterFilter_NoParallelism, imageAfterExtendedFilter_NoParallelism)
        Assert.Equal(imageAfterFilter_PixelParallelism, imageAfterExtendedFilter_PixelParallelism)
        Assert.Equal(imageAfterFilter_PartsParallelism, imageAfterExtendedFilter_PartsParallelism)
        Assert.Equal(imageAfterFilter_RowParallelism, imageAfterExtendedFilter_RowParallelism)
        Assert.Equal(imageAfterFilter_ColParallelism, imageAfterExtendedFilter_ColParallelism)

    [<Property(Arbitrary = [| typeof<Image100> |])>]
    member _.someAreCommutative (image: Async<Rgba32[, ]>) =
        let image12_NoParallelism = applyFilterNoParallelismA shiftRight image |> applyFilterNoParallelismA shiftDown
        let image21_NoParallelism = applyFilterNoParallelismA shiftDown image |> applyFilterNoParallelismA shiftRight

        let image12_PixelParallelism = applyFilterPixelParallelismA shiftRight image |> applyFilterPixelParallelismA shiftDown
        let image21_PixelParallelism = applyFilterPixelParallelismA shiftDown image |> applyFilterPixelParallelismA shiftRight

        let image12_PartsParallelism = applyFilterParallelismInPartsA shiftRight image |> applyFilterParallelismInPartsA shiftDown
        let image21_PartsParallelism = applyFilterParallelismInPartsA shiftDown image |> applyFilterParallelismInPartsA shiftRight

        let image12_RowParallelism = applyFilterRowParallelismA shiftRight image |> applyFilterRowParallelismA shiftDown
        let image21_RowParallelism = applyFilterRowParallelismA shiftDown image |> applyFilterRowParallelismA shiftRight

        let image12_ColParallelism = applyFilterColParallelismA shiftRight image |> applyFilterColParallelismA shiftDown
        let image21_ColParallelism = applyFilterColParallelismA shiftDown image |> applyFilterColParallelismA shiftRight
        
        Assert.Equal(image12_NoParallelism, image21_NoParallelism)
        Assert.Equal(image12_PixelParallelism, image21_PixelParallelism)
        Assert.Equal(image12_PartsParallelism, image21_PartsParallelism)
        Assert.Equal(image12_RowParallelism, image21_RowParallelism)
        Assert.Equal(image12_ColParallelism, image21_ColParallelism)
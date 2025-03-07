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
        let imageAfterFilter = applyFilter gaussianBlur image
        Assert.Equal(100, imageAfterFilter.GetLength(0))
        Assert.Equal(100, imageAfterFilter.GetLength(1))

    [<Property(Arbitrary = [| typeof<Image100> |])>]
    member _.idDoesntChangeData (image: Rgba32[,]) =
        let imageAfterFilter = applyFilter id image
        Assert.Equal(image, imageAfterFilter)

    [<Property(Arbitrary = [| typeof<Image2> |])>]
    member _.imageSmallerThanFilter (image: Rgba32[,]) =
        let imageAfterFilter = applyFilter black image
        Assert.Equal(true, imageIsBlack imageAfterFilter)

    [<Property(Arbitrary = [| typeof<Image100> |])>]
    member _.blackFilter (image: Rgba32[,]) =
        let imageBlack = applyFilter black image
        Assert.Equal(true, imageIsBlack imageBlack)

    [<Property(Arbitrary = [| typeof<Image100> |])>]
    member _.shiftComposition (image: Rgba32[,]) =
        let trivial1and2 = applyFilter shiftRight image |> applyFilter shiftDown
        let trivial12 = applyFilter shiftDiagonal image
        Assert.Equal(trivial1and2, trivial12)

    [<Property(Arbitrary = [| typeof<Image100> |])>]
    member _.extendedComposition (image: Rgba32[,])  =
        let imageAfterFilter = applyFilter kernel image
        let imageAfterExtendedFilter = applyFilter kernelExtended image
        Assert.Equal(imageAfterFilter, imageAfterExtendedFilter)

    [<Property(Arbitrary = [| typeof<Image100> |])>]
    member _.someAreCommutative (image: Rgba32[,]) =
        let image12 = applyFilter shiftRight image |> applyFilter shiftDown
        let image21 = applyFilter shiftDown image |> applyFilter shiftRight
        Assert.Equal(image12, image21)
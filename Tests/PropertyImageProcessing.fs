namespace PropertyImageProcessing

open Xunit
open FsCheck
open FsCheck.Xunit
open SixLabors.ImageSharp.PixelFormats

open ImageProcessing.IProcessing
open UnitImageProcessing.Data


type Image() =
    static member Rgba32() =
        let pixelGen : Gen<Rgba32> =
            gen {
                let! r = Gen.choose(0, 255) |> Gen.map byte
                let! g = Gen.choose(0, 255) |> Gen.map byte
                let! b = Gen.choose(0, 255) |> Gen.map byte
                let a = byte 255
                return Rgba32(r, g, b, a)
            }

        let array2DGen : Gen<Rgba32[,]> =
            gen {
                let! pixels = Gen.array2DOfDim (100, 100) pixelGen
                return pixels
            }

        Arb.fromGen array2DGen

type SmallImage() =
    static member Rgba32() =
        let pixelGen : Gen<Rgba32> =
            gen {
                let! r = Gen.choose(0, 255) |> Gen.map byte
                let! g = Gen.choose(0, 255) |> Gen.map byte
                let! b = Gen.choose(0, 255) |> Gen.map byte
                let a = byte 255 
                return Rgba32(r, g, b, a)
            }

        let array2DGen : Gen<Rgba32[,]> =
            gen {
                let! pixels = Gen.array2DOfDim (2, 2) pixelGen
                return pixels
            }

        Arb.fromGen array2DGen


[<Properties(MaxTest = 100)>]
type Filter() =

    [<Property(Arbitrary = [| typeof<Image> |])>]
    member _.filterDoesntChangeSize (image: Rgba32[,]) =
        let imageAfterFilter = applyFilter gaussianBlur image
        Assert.Equal(image.GetLength(0), imageAfterFilter.GetLength(0))
        Assert.Equal(image.GetLength(1), imageAfterFilter.GetLength(1))

    [<Property(Arbitrary = [| typeof<Image> |])>]
    member _.idDoesntChangeData (image: Rgba32[,]) =
        let imageAfterFilter = applyFilter id image
        Assert.Equal(image, imageAfterFilter)

    [<Property(Arbitrary = [| typeof<SmallImage> |])>]
    member _.imageSmallerThanFilter (image: Rgba32[,]) =
        let imageAfterFilter = applyFilter black image
        Assert.Equal(true, imageIsBlack imageAfterFilter)

    [<Property(Arbitrary = [| typeof<Image> |])>]
    member _.blackFilter (image: Rgba32[,]) =
        let imageBlack = applyFilter black image
        Assert.Equal(true, imageIsBlack imageBlack)

    [<Property(Arbitrary = [| typeof<Image> |])>]
    member _.trivialComposition (image: Rgba32[,]) =
        let trivial1and2 = applyFilter kernelTrivial1 image |> applyFilter kernelTrivial2
        let trivial12 = applyFilter kernelTrivial12 image
        Assert.Equal(trivial1and2, trivial12)

    [<Property(Arbitrary = [| typeof<Image> |])>]
    member _.extendedComposition (image: Rgba32[,])  =
        let imageAfterFilter = applyFilter kernel image
        let imageAfterExtendedFilter = applyFilter kernelExtended image
        Assert.Equal(imageAfterFilter, imageAfterExtendedFilter)

    [<Property(Arbitrary = [| typeof<Image> |])>]
    member _.someAreCommutative (image: Rgba32[,]) =
        let image12 = applyFilter kernelTrivial1 image |> applyFilter kernelTrivial2
        let image21 = applyFilter kernelTrivial2 image |> applyFilter kernelTrivial1
        Assert.Equal(image12, image21)
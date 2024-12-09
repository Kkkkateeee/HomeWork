namespace PropertyQtrees

open Xunit
open FsCheck
open FsCheck.Xunit

open QTrees
open QTrees.QTrees


[<Properties(MaxTest = 100)>]
type mapProp() =
    let a = 1
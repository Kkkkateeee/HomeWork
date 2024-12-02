namespace PropertyQtrees

open Xunit
open FsCheck
open FsCheck.Xunit

open Q_Trees
open Q_Trees.QTrees


[<Properties(MaxTest = 10)>]
type mapProp() =
    let a = 1
module properties

open FsCheck

let prop_SortedList (xs: int list) =
    let sorted = List.sort xs
    List.forall2 (<=) sorted (List.tail sorted)

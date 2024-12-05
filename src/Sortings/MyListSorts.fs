namespace MyListSorts


type MyList<'t> =
    | Empty
    | Cons of 't * MyList<'t>

module MyList =

    let rec fromSystemList (list: 't List) : (MyList<'t>) =
        match list with
        | [] -> Empty
        | head :: tail -> Cons(head, fromSystemList tail)

    let rec toSystemList (list: MyList<'t>) : ('t List) =
        match list with
        | Empty -> []
        | Cons(head, tail) -> head :: toSystemList tail


    let bubbleSort (list: MyList<'t>) : MyList<'t> =
        let rec bubble list swapped =
            match list with
            | Empty -> (Empty, swapped)
            | Cons(x, Empty) -> (Cons(x, Empty), swapped)
            | Cons(x1, Cons(x2, other)) ->

                if compare x1 x2 > 0 then
                    let (sortedTail, _) = bubble (Cons(x1, other)) true
                    (Cons(x2, sortedTail), true)

                else
                    let (sortedTail, s) = bubble (Cons(x2, other)) swapped
                    (Cons(x1, sortedTail), s)

        let rec sort list =
            let (newList, swapped) = bubble list false
            if swapped then sort newList else newList

        sort list


    let rec quickSort (list: MyList<'t>) : MyList<'t> when 't : comparison =
        let myFilter (pred: 't -> bool) (list: MyList<'t>) : MyList<'t> =
            let rec f (lst: MyList<'t>) (acc: MyList<'t>) =
                match lst with 
                | Empty -> acc
                | Cons(head, tail) ->
                    if pred head then 
                        f tail (Cons(head, acc))
                    else 
                        f tail acc
            f list Empty
            
        let rec append xs ys =
            match xs with
            | Empty -> ys
            | Cons(x, xs') -> Cons(x, append xs' ys)

        match list with
        | Empty -> Empty
        | Cons(pivot, other) ->
            let smaller = myFilter((>) pivot) other
            let larger = myFilter ((<=) pivot) other
            append (quickSort smaller) (Cons(pivot, quickSort larger))


    let rec mergeSort (list: MyList<'t>) : MyList<'t> =
        let rec merge (leftList: MyList<'t>) (rightList: MyList<'t>) =
            match leftList, rightList with
            | Empty, _ -> rightList
            | _, Empty -> leftList
            | Cons(x, xs), Cons(y, ys) ->
                if compare x y <= 0 then
                    Cons(x, (merge xs rightList))
                else
                    Cons(y, (merge leftList ys))

        let separate (list: MyList<'t>) : (MyList<'t> * MyList<'t>) =
            let rec separateTailRec (lst: MyList<'t>) (left: MyList<'t>) (right: MyList<'t>) : (MyList<'t> * MyList<'t>) =
                match lst with
                | Empty -> (left, right)
                | Cons(x, Empty) -> (Cons(x, left), right)
                | Cons(x, Cons(y, other)) ->
                    separateTailRec other (Cons(x, left)) (Cons(y, right))
            
            separateTailRec list Empty Empty

        match list with
        | Empty -> Empty
        | Cons(x, Empty) -> Cons(x, Empty)
        | _ ->
            let leftList, rightList = separate list
            merge (mergeSort leftList) (mergeSort rightList)
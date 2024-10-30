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
            if swapped then sort newList 
             else newList

        sort list



    let rec quickSort (list: MyList<'t>) : MyList<'t> =
        let rec partition pivot list =
            match list with
            | Empty -> (Empty, Empty, Empty)
            | Cons(head, tail) ->
                let smaller, equal, larger = partition pivot tail

                if compare head pivot < 0 then
                    (Cons(head, smaller), equal, larger)
                elif head = pivot then
                    (smaller, Cons(head, equal), larger)
                else
                    (smaller, equal, Cons(head, larger))

        let rec append xs ys =
            match xs with
            | Empty -> ys
            | Cons(x, xs') -> Cons(x, append xs' ys)

        match list with
        | Empty -> Empty
        | Cons(pivot, other) ->
            let smaller, equal, larger = partition pivot other
            append (quickSort smaller) (append equal (Cons(pivot, quickSort larger)))


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

        let rec separate (list: MyList<'t>) : (MyList<'t> * MyList<'t>) =
            match list with
            | Empty -> (Empty, Empty)
            | Cons(x, Empty) -> (Cons(x, Empty), Empty)
            | Cons(x, Cons(y, other)) ->
                let (leftList, rightList) = separate other
                (Cons(x, leftList), Cons(y, rightList))

        match list with
        | Empty -> Empty
        | Cons(x, Empty) -> Cons(x, Empty)
        | _ ->
            let leftList, rightList = separate list
            merge (mergeSort leftList) (mergeSort rightList)


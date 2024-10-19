namespace MyListSorts


type MyList<'t> =
    | Empty
    | Cons of 't * MyList<'t>


module TypeConvertFunctions =

    let rec ConvertListToMyList (list: 't List) : (MyList<'t>) =
        match list with
        | [] -> Empty
        | head :: tail -> Cons(head, ConvertListToMyList tail)

    let rec ConvertMyListToList (list: MyList<'t>) : ('t List) =
        match list with
        | Empty -> []
        | Cons(head, tail) -> head :: ConvertMyListToList tail


module SortFunctions =

    let rec bubble_sort (list: MyList<'t>) : MyList<'t> =
        let rec bubble list =
            match list with
            | Empty -> Empty
            | Cons(x, Empty) -> Cons(x, Empty)
            | Cons(x1, Cons(x2, other)) ->
                if compare x1 x2 > 0 then
                    Cons(x2, bubble (Cons(x1, other)))
                else
                    Cons(x1, bubble (Cons(x2, other)))

        let sorted_list = bubble list

        if sorted_list = list then
            sorted_list
        else
            bubble_sort sorted_list


    let rec quick_sort (list: MyList<'t>) : MyList<'t> =
        match list with
        | Empty -> Empty
        | Cons(pivot, other) ->
            let smaller, equal, larger = partition pivot other
            append (quick_sort smaller) (append equal (Cons(pivot, quick_sort larger)))

    and partition pivot list =
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

    and append xs ys =
        match xs with
        | Empty -> ys
        | Cons(x, xs') -> Cons(x, append xs' ys)


    let rec merge_sort (list: MyList<'t>) : MyList<'t> =
        match list with
        | Empty -> Empty
        | Cons(x, Empty) -> Cons(x, Empty)
        | _ ->
            let left_list, right_list = separate list
            merge (merge_sort left_list) (merge_sort right_list)

    and merge (left_list: MyList<'t>) (right_list: MyList<'t>) =
        match left_list, right_list with
        | Empty, _ -> right_list
        | _, Empty -> left_list
        | Cons(x, xs), Cons(y, ys) ->
            if compare x y <= 0 then
                Cons(x, (merge xs right_list))
            else
                Cons(y, (merge left_list ys))

    and separate (list: MyList<'t>) : (MyList<'t> * MyList<'t>) =
        match list with
        | Empty -> (Empty, Empty)
        | Cons(x, Empty) -> (Cons(x, Empty), Empty)
        | Cons(x, Cons(y, other)) ->
            let (left_list, right_list) = separate other
            (Cons(x, left_list), Cons(y, right_list))
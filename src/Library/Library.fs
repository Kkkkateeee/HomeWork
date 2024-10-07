namespace Library


module bubble =
     let rec bubble_sort (list: 't list) =
        let rec bubble list =
            match list with 
            | [] -> []
            | [x] -> [x]
            | x1 :: x2 :: other ->
                if compare x1 x2 > 0 then 
                    x2 :: bubble (x1 :: other)  
                else
                    x1 :: bubble (x2 :: other)  

        let sorted_list = bubble list
        if sorted_list = list then
            sorted_list  
        else
            bubble_sort sorted_list  


module quick =
    let rec quick_sort (list: 't list) =
        match list with
        | [] -> []
        | pivot :: other ->
            let smaller = List.filter(fun x -> compare x pivot < 0) other in
            let equal = List.filter(fun x -> compare x pivot = 0) other in 
            let larger = List.filter(fun x -> compare x pivot > 0) other in
            quick_sort smaller @ (pivot :: equal) @ quick_sort larger


module merge =
    
    let rec merge (left_list: 't list) (right_list: 't list) =
        match left_list, right_list with
        | [], _ -> right_list
        | _, [] -> left_list
        | x :: xs, y :: ys ->
            if compare x y <= 0 then
                x :: merge xs right_list
            else
                y :: merge left_list ys
    
    let rec separate (list: 't list) : ('t list * 't list) = 
        match list with
        | [] -> ([], [])
        | [x] -> ([x], [])
        | x :: y :: other ->
            let (left_list, right_list) = separate other
            (x :: left_list, y :: right_list)
 
    let rec merge_sort (list: 't list) =
        match list with
        | [] -> []
        | [x] -> [x]
        | _ ->
            let left_list, right_list = separate list
            merge (merge_sort left_list)(merge_sort right_list)

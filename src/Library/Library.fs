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


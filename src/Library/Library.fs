namespace Library


module sorts =
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

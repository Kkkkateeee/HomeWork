namespace Library


module sorts =
    let buble_sort list =
        let buble list swap =
            match list with 
            | []
            | [x] -> [x]
            | x1 :: x2 :: other ->
                if x1 > x2 then
                    x2 :: buble
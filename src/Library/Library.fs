namespace Library

module bubble_sort =
    let bubble_sort (array : int[]) =
    
        let mutable array_len = array.Length
        let mutable swapped = true
    
        while swapped do
            swapped <- false 
            for i in 0 .. array_len - 2 do
                if array[i] > array[i + 1] then
                    let elem = array[i]
                    array[i] <- array[i + 1]
                    array[i + 1] <- elem
                    swapped <- true 
            array_len <- array_len - 1 


namespace ArraySorts


module sorts =

    let swap (arr: 't[]) i j =
        let elem = arr[i]
        arr[i] <- arr[j]
        arr[j] <- elem


    let bubble_sort (arr: 't[]) : 't[] =

        let mutable array_len = arr.Length
        let mutable swaped = true

        while swaped do
            swaped <- false

            for i in 0 .. array_len - 2 do
                if compare arr[i] arr[i + 1] > 0 then
                    swap arr i (i + 1)
                    swaped <- true

        array_len <- array_len - 1
        arr


    let partition (arr: 't[]) (low: int) (high: int) =
        let pivot = arr[high]
        let mutable i = low - 1

        for j in low .. (high - 1) do
            if compare arr[j] pivot <= 0 then
                i <- i + 1
                swap arr i j

        swap arr (i + 1) high
        i + 1

    let rec inplace_quick (arr: 't[]) (low: int) (high: int) =
        if low < high then
            let pivotIndex = partition arr low high
            inplace_quick arr low (pivotIndex - 1)
            inplace_quick arr (pivotIndex + 1) high

    let quick_sort (arr: 't[]) : 't[] =
        inplace_quick arr 0 (Array.length arr - 1)
        arr


    let merge (arr: 't[]) (low: byref<int>) (high: byref<int>) (mid: byref<int>) =
        let mutable start2 = mid + 1

        if compare arr[mid] arr[start2] > 0 then
            while low <= mid && start2 <= high do
                if compare arr[low] arr[start2] <= 0 then
                    low <- low + 1
                else
                    let value = arr[start2]
                    let mutable index = start2

                    while index <> low do
                        arr[index] <- arr[index - 1]
                        index <- index - 1

                    arr[low] <- value

                    low <- low + 1
                    mid <- mid + 1
                    start2 <- start2 + 1

    let rec inplace_merge (arr: 't[]) (low: byref<int>) (high: byref<int>) =
        if low < high then
            let mutable mid = (low + high) / 2
            let mutable mid_plus_1 = mid + 1
            inplace_merge arr &low &mid
            inplace_merge arr &mid_plus_1 &high
            merge arr &low &high &mid

    let merge_sort (arr: 't[]) : 't[] =
        let mutable low = 0
        let mutable high = Array.length arr - 1
        inplace_merge arr &low &high
        arr
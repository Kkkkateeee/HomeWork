namespace ArraySorts


module ArraySorts =

    let swap (arr: 't[]) i j =
        let elem = arr[i]
        arr[i] <- arr[j]
        arr[j] <- elem


    let bubbleSort (arr: 't[]) : 't[] =

        let mutable arrayLen = arr.Length
        let mutable swaped = true

        while swaped do
            swaped <- false

            for i in 0 .. arrayLen - 2 do
                if compare arr[i] arr[i + 1] > 0 then
                    swap arr i (i + 1)
                    swaped <- true

        arrayLen <- arrayLen - 1
        arr


    let quickSort (arr: 't[]) : 't[] =
        let partition (arr: 't[]) (low: int) (high: int) =
            let pivot = arr[high]
            let mutable i = low - 1

            for j in low .. (high - 1) do
                if compare arr[j] pivot <= 0 then
                    i <- i + 1
                    swap arr i j

            swap arr (i + 1) high
            i + 1

        let rec inplaceQuick (arr: 't[]) (low: int) (high: int) =
            if low < high then
                let pivotIndex = partition arr low high
                inplaceQuick arr low (pivotIndex - 1)
                inplaceQuick arr (pivotIndex + 1) high

        inplaceQuick arr 0 (Array.length arr - 1)
        arr


    let merge (arr: 't[]) (low: byref<int>) (high: byref<int>) (mid: byref<int>) =
        let mutable start2 = mid + 1
        let mutable start1 = low

        if compare arr[mid] arr[start2] > 0 then
            while start1 <= mid && start2 <= high do
                if compare arr[start1] arr[start2] <= 0 then
                    start1 <- start1 + 1
                else
                    let value = arr[start2]
                    let mutable index = start2

                    while index > start1 do
                        arr[index] <- arr[index - 1]
                        index <- index - 1

                    arr[start1] <- value

                    start1 <- start1 + 1
                    mid <- mid + 1
                    start2 <- start2 + 1

    let rec inplaceMerge (arr: 't[]) (low: byref<int>) (high: byref<int>) =
        if low < high then
            let mutable mid = (low + high) / 2
            let mutable midPlus1 = mid + 1
            inplaceMerge arr &low &mid
            inplaceMerge arr &midPlus1 &high
            merge arr &low &high &mid

    let mergeSort (arr: 't[]) : 't[] =
        let mutable low = 0
        let mutable high = Array.length arr - 1
        inplaceMerge arr &low &high
        arr

namespace Library


module factorial =
    let rec factorial n =
        if n < 0 then
            failwith "The factorial of negative numbers is not calculated by this program\n"
        elif n = 0 || n = 1 then
            1
        else
            n * factorial (n - 1)


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


module fibinacci =

    type Matrix = int64 array array

    let multiply (m1: Matrix) (m2: Matrix) : Matrix =
        [| [| m1.[0].[0] * m2.[0].[0] + m1.[0].[1] * m2.[1].[0]
              m1.[0].[0] * m2.[0].[1] + m1.[0].[1] * m2.[1].[1] |]
           [| m1.[1].[0] * m2.[0].[0] + m1.[1].[1] * m2.[1].[0]
              m1.[1].[0] * m2.[0].[1] + m1.[1].[1] * m2.[1].[1] |] |]

    let rec power (matrix: Matrix) (n: int) : Matrix =
        if n = 1 then
            matrix
        else
            let halfPower = power matrix (n / 2)
            let result = multiply halfPower halfPower
            if n % 2 = 0 then result else multiply result matrix

    let fibonacci (n: int) : int64 =
        if n < 0 then
            failwith "Fibonacci numbers for negative n are not defined\n"

        if n = 0 then
            0L
        elif n = 1 then
            1L
        else
            let mainMatrix = [| [| 1L; 1L |]; [| 1L; 0L |] |]
            let resultMatrix = power mainMatrix (n - 1)
            resultMatrix.[0].[0]

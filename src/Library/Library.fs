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


    let bubble_sort (arr: 't[]) =

        let mutable array_len = arr.Length
        let mutable swapped = true

        while swapped do
            swapped <- false

            for i in 0 .. array_len - 2 do
                if compare arr[i] arr[i + 1] > 0 then
                    swap arr i (i + 1)
                    swapped <- true

            array_len <- array_len - 1


    let rec quick_sort (arr: 't[]) left right =

        if left < right then
            let pivot = arr[(left + right) / 2]
            let mutable i = left
            let mutable j = right

            while i <= j do
                while compare arr[i] pivot < 0 do
                    i <- i + 1

                while compare arr[j] pivot > 0 do
                    j <- j - 1

                if i <= j then
                    swap arr i j
                    i <- i + 1
                    j <- j - 1

            if left < j then
                quick_sort arr left j

            if i < right then
                quick_sort arr i right


    let rec merge (arr: 'a[]) (left: int ref) (mid: int ref) (right: int ref) =
        let mutable start2 = mid.Value + 1

        if compare arr.[mid.Value] arr.[start2] > 0 then
            ()
        else
            while left.Value <= mid.Value && start2 <= right.Value do
                if compare arr.[mid.Value] arr.[start2] > 0 then
                    left.Value <- left.Value + 1
                else
                    let value = arr.[start2]
                    let mutable index = start2

                    while index > left.Value do
                        swap arr index (index - 1)
                        index <- index - 1

                    arr.[left.Value] <- value

                    left.Value <- left.Value + 1
                    mid.Value <- mid.Value + 1
                    start2 <- start2 + 1

    let rec merge_sort (arr: 't[]) (left: int) (right: int) =
        if left < right then
            let mid = (right + left) / 2

            merge_sort arr left mid
            merge_sort arr (mid + 1) right

            let leftRef = ref left
            let midRef = ref mid
            let rightRef = ref right

            merge arr leftRef midRef rightRef


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

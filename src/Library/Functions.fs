namespace Functions


module factorial =
    let rec factorial n =
        if n < 0 then
            failwith "The factorial of negative numbers is not calculated by this program\n"
        elif n = 0 || n = 1 then
            1
        else
            n * factorial (n - 1)


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
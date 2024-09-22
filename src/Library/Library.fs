﻿namespace Library

module Say =

    // объявление типа матрицы
    type Matrix = int64 array array  


    // Функция для умножения двух матриц
    let multiply (m1: Matrix) (m2: Matrix) : Matrix =
        [| 
            [| m1.[0].[0] * m2.[0].[0] + m1.[0].[1] * m2.[1].[0];
               m1.[0].[0] * m2.[0].[1] + m1.[0].[1] * m2.[1].[1] |];
            [| m1.[1].[0] * m2.[0].[0] + m1.[1].[1] * m2.[1].[0];
               m1.[1].[0] * m2.[0].[1] + m1.[1].[1] * m2.[1].[1] |]
        |]
        

    // Функция для возведения матрицы в степень n
    let rec power (matrix: Matrix) (n: int) : Matrix =
        if n = 1 then matrix
        else
            let halfPower = power matrix (n / 2)
            let result = multiply halfPower halfPower
            if n % 2 = 0 then result
            else multiply result matrix


    // Функция для получения n-го числа Фибоначчи
    let fibonacci (n: int) : int64 =
        if n < 0 then failwith "Числа Фибоначчи для отрицательных n не определены\n"
        if n = 0 then 0L
        elif n = 1 then 1L
        else
            let mainMatrix = [| [| 1L; 1L |]; [| 1L; 0L |] |]
            let resultMatrix = power mainMatrix (n - 1)
            resultMatrix.[0].[0]
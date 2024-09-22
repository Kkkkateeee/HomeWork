namespace Library

module Say =
    let rec factorial n =
        if n < 0 then failwith "Факториал отрицательных чисел не вычисляется этой программой\n"
        elif n = 0 || n = 1 then 1
        else n*factorial (n - 1)
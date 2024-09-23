namespace Library

module factorial =
    let rec factorial n =
        if n < 0 then failwith "The factorial of negative numbers is not calculated by this program\n"
        elif n = 0 || n = 1 then 1
        else n*factorial (n - 1)
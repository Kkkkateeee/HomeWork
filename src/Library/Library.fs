namespace Library

module quick_sort =
    let rec Quick_sort (arr: int[]) =

        if Array.length arr <= 1  then arr

        else
            let pivot = Array.last arr  // последний элемент массива

            let smaller = ResizeArray<int>()
            let bigger = ResizeArray<int>()
            let equal = ResizeArray<int>()

            for i in 0 .. Array.length arr - 1 do
                if arr[i] < pivot then smaller.Add(arr[i])
                elif arr[i] > pivot then bigger.Add(arr[i])
                else equal.Add(arr[i])
            
            Array.append (Array.append (Quick_sort (smaller.ToArray()) ) (equal.ToArray())) (Quick_sort (bigger.ToArray()))
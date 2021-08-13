module UtilitiesSIMD

    open System
    open System.Numerics

    module ArraySIMD = 
        let inline map2
            (vf : ^T Vector -> ^U Vector -> ^V Vector) 
            (sf : ^T -> ^U -> ^V)
            (array1 : ^T[]) 
            (array2 : ^U[]) : ^V[] =

            let count = Vector< ^T>.Count    
            if count <> Vector< ^U>.Count || count <> Vector< ^V>.Count then invalidArg "array" "Inputs and output must all have same Vector width."

            let len = array1.Length        
            if len <> array2.Length then invalidArg "array2" "Arrays must have same length"

            let result = Array.zeroCreate len

            let mutable i = 0    
            while i <= len-count do
                (vf (Vector< ^T>(array1,i)) (Vector< ^U>(array2,i))).CopyTo(result,i)   
                i <- i + count

            i <- len-len%count
            while i < len do
                result.[i] <- sf array1.[i] array2.[i]
                i <- i + 1
            result

        let inline map
            (vf : ^T Vector -> ^U Vector) (sf : ^T -> ^U) (array : ^T[]) : ^U[] =
            let len = array.Length
            let count = Vector< ^T>.Count
            if count <> Vector< ^U>.Count then invalidArg "array" "Output type must have the same width as input type."    

            let result = Array.zeroCreate len

            let mutable i = 0
            while i <= len-count do        
                (vf (Vector< ^T>(array,i ))).CopyTo(result,i)   
                i <- i + count

            i <- len-len%count
            while i < len do
                result.[i] <- sf array.[i]
                i <- i + 1
            result

        let inline repeat (n:int) (array: ^T[]) = 
            let len = array.Length
            let count = Vector< ^T>.Count

            let result = Array.zeroCreate (len * n)

            let mutable j = 0
            while j < n do
                let mutable i = 0
                while i <= len-count do
                    Vector< ^T>(array,i).CopyTo(result,j*len + i)
                    i <- i + count
            
                i <- len-len%count
                while i < len do
                    result.[j*len + i] <- array.[i]
                    i <- i + 1
                j <- j + 1
            result

        let inline add (array1:^T[]) (array2:^T[]) : ^T[] = map2 (fun x y -> x + y) (fun x y -> x + y) array1 array2
        let inline substract (array1:^T[]) (array2:^T[]) : ^T[] = map2 (fun x y -> x - y) (fun x y -> x - y) array1 array2
        let inline mult (array1:^T[]) (array2:^T[]) : ^T[] = map2 (fun x y -> x * y) (fun x y -> x * y) array1 array2

        let inline addScalar (x:^T) (array:^T[]) = map (fun v -> v + Vector< ^T>(x)) (fun i -> i + x) array
        let inline substractScalar (x:^T) (array:^T[]) = map (fun v -> v - Vector< ^T>(x)) (fun i -> i - x) array
        let inline multScalar (x:^T) (array:^T[]) = map (fun v -> v * Vector< ^T>(x)) (fun i -> i * x) array

    module MatrixSIMD = 
        let inline flatten (M1: ^T[,]) : ^T[] = 
            [| for x in [0..(Array2D.length1 M1)-1] do
                    for y in [0..(Array2D.length2 M1)-1] do
                        M1.[x,y] |]

        let inline fromArray (d1:int) (d2:int) (array:^T[]) : ^T[,] = 
            if (d1*d2) <> array.Length then invalidArg "array" "Cannot make a matrix from this array with the dimensions given."
            Array2D.init d1 d2 (fun i j -> array.[j*d1 + i])

        let inline map2
            (vf : ^T Vector -> ^U Vector -> ^V Vector) 
            (sf : ^T -> ^U -> ^V)
            (array1 : ^T[,]) 
            (array2 : ^U[,]) : ^V[,] = 

            if Array2D.length1 array1 <> Array2D.length1 array2 || Array2D.length2 array1 <> Array2D.length2 array2 then invalidArg "matrix" "Matrix don't have the same shape."

            let flattenArray1 = flatten array1
            let flattenArray2 = flatten array2

            let result = ArraySIMD.map2 vf sf flattenArray1 flattenArray2
            fromArray (Array2D.length1 array1) (Array2D.length2 array1) result

        let inline map
            (vf : ^T Vector -> ^V Vector) (sf : ^T  -> ^V) (array : ^T[,]) : ^V[,] = 
            array |> flatten
                  |> ArraySIMD.map vf sf
                  |> fromArray (Array2D.length1 array) (Array2D.length2 array)

        let inline add (M1: ^T[,]) (M2: ^T[,]) : ^T[,] = map2 (fun x y -> x + y) (fun x y -> x + y) M1 M2
        let inline substract (M1: ^T[,]) (M2: ^T[,]) : ^T[,] = map2 (fun x y -> x - y) (fun x y -> x - y) M1 M2
        let inline mult (M1: ^T[,]) (M2: ^T[,]) : ^T[,] = map2 (fun x y -> x * y) (fun x y -> x * y) M1 M2

        let inline addScalar (x: ^T) (M: ^T[,]) = map (fun v -> v + Vector< ^T>(x)) (fun i -> i + x) M
        let inline substractScalar (x: ^T) (M: ^T[,]) = map (fun v -> v - Vector< ^T>(x)) (fun i -> i - x) M
        let inline multScalar (x: ^T) (M: ^T[,]) = map (fun v -> v * Vector< ^T>(x)) (fun i -> i * x) M





            


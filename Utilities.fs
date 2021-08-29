module Utilities

    let dot a b = Array.fold2 (fun state x y -> state + x * y) 0.0 a b

    let cov (array1: float array) (array2:float array) =  // Only valid for zero mean processes.
        (dot array1 array2) / (float (array1.Length - 1))

    let autoCovAt (lag:int) (array:float array) = 
        cov (Array.skip lag array) (Array.truncate (array.Length - lag) array)

    let autocovariance (max_lag:int) (array: float array) = 
        Array.zeroCreate max_lag |> Array.mapi (fun i _ -> autoCovAt i array)

    let autocorrelation (max_lag:int) (array: float array) = 
        let a = autocovariance max_lag array
        a |> Array.map (fun x -> x / a.[0])

    let listOfListToArray ll = ll |> Array.ofList |> Array.map (fun l -> l |> Array.ofList)

    let cartesianProductList ll = List.foldBack (fun n g -> [for n' in n do for g' in g do yield n' :: g']) ll [[]]
    let cartesianProductArray arrayOfArray = arrayOfArray |> Array.toList |> cartesianProductList |> listOfListToArray
    
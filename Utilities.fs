module Utilities
    open MathNet.Numerics
    open MathNet.Numerics.IntegralTransforms

    let rand = new System.Random()

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

    let extractFst arrayTuples = arrayTuples |> Array.map (fun (x1,_) -> x1)
    let extractSnd arrayTuples = arrayTuples |> Array.map (fun (_,x2) -> x2)

    let swap x y (a: _ array) =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    let shuffle array = Array.iteri (fun i _ -> array |> swap i (rand.Next(i,array.Length))) array; array
        
    let matchSizeOf (array1:'T[]) (array2:'U[]) = 
        if array1.Length >= array2.Length then
            array1,
            Array.concat [|array2; Array.zeroCreate (array1.Length - array2.Length)|]
        else 
            Array.concat [|array1; Array.zeroCreate (array2.Length - array1.Length)|],
            array2

    let fft (array:complex[]) = 
        let out = Array.copy array
        Fourier.Forward(out, FourierOptions.AsymmetricScaling)
        out

    let ifft (array:complex[]) = 
        let out = Array.copy array
        Fourier.Inverse(out, FourierOptions.AsymmetricScaling)
        out

    let toComplex = Array.map (fun x -> complex x 0.0)

    let convolution (signal:float[]) (filter:float[]) = 
        let signalF, filterF = matchSizeOf (Array.copy signal) (Array.copy filter)
        let signalF = signalF |> toComplex
        let filterF = filterF |> toComplex
        Fourier.Forward(signalF, FourierOptions.AsymmetricScaling)
        Fourier.Forward(filterF, FourierOptions.AsymmetricScaling)
        let convolution = Array.map2 (fun s f -> Complex.mul s f) signalF filterF
        Fourier.Inverse(convolution, FourierOptions.AsymmetricScaling)
        convolution |> Array.map (fun x -> Complex.realPart x)

    let roll rightShift (array:'T[]) = 
        let shift = if rightShift >= 0 then array.Length - rightShift else -rightShift
        let arr1, arr2 = array |> Array.splitAt shift
        Array.concat [|arr2;arr1|]

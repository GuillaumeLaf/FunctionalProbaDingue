[<AutoOpen>]
module Utils 
    
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.Distributions

    let mapTuple f (x,y) = (f x, f y)

    let randomNormalVector length lowerCholesky = ( * ) (Matrix<float32>.Build.DenseOfArray(lowerCholesky))
                                                        (Vector<float32>.Build.Random(length, new Normal())) |> Vector.toArray

    // https://rosettacode.org/wiki/Cholesky_decomposition#F.23
    let cholesky a =
        let calc (a: float32[,]) (l: float32[,]) i j =
            let c1 j =
                let sum = List.sumBy (fun k -> l.[j, k] ** 2.0f) [0..j - 1]
                sqrt (a.[j, j] - sum)
            let c2 i j = 
                let sum = List.sumBy (fun k -> l.[i, k] * l.[j, k]) [0..j - 1]
                (1.0f / l.[j, j]) * (a.[i, j] - sum)
            if j > i then 0.0f else
                if i = j
                then c1 j
                else c2 i j
        let l = Array2D.zeroCreate (Array2D.length1 a) (Array2D.length2 a)
        Array2D.iteri (fun i j _ -> l.[i, j] <- calc a l i j) l
        l

(*    let dot a b = Array.fold2 (fun state x y -> state + x * y) 0.0 a b

    let cov (array1: float array) (array2:float array) =  // Only valid for zero mean processes.
        (dot array1 array2) / (float (array1.Length - 1))

    let autoCovAt (lag:int) (array:float array) = 
        cov (Array.skip lag array) (Array.truncate (array.Length - lag) array)

    let autocovariance (max_lag:int) (array: float array) = 
        Array.zeroCreate max_lag |> Array.mapi (fun i _ -> autoCovAt i array)

    let autocorrelation (max_lag:int) (array: float array) = 
        let a = autocovariance max_lag array
        a |> Array.map (fun x -> x / a.[0])*)

    module Array2D = 
        let length (array:'T[,]) = Array2D.length1 array * Array2D.length2 array
        let row idx (array:'T[,]) = array.[idx,*]
        let setRow idx values (array:'T[,]) = array.[idx,*] <- values; array
        let col idx (array:'T[,]) = array.[*,idx]
        let setCol idx values (array:'T[,]) = array.[*,idx] <- values; array

        let collectByRow f (array:'T[,]) = Array.mapi (fun i _ -> f array.[i,*]) array.[*,0]
        let collectByCol f (array:'T[,]) = Array.mapi (fun i _ -> f array.[*,i]) array.[0,*]

        let ofSingleArray (array:'T[]) = Array2D.init array.Length 1 (fun i _ -> array.[i])

        // Fst : outer array
        // Snd : inner array
        let ofArray (array:'T[][]) = 
            if (Array.concat >> Array.length) array % array.[0].Length = 0 then     
                Array2D.init array.Length array.[0].Length (fun i j -> array.[i].[j])
            else invalidArg "array" "Jagged array cannot be casted into Array2D. At least one inner array hasn't the right size."

        let flatten (array:'T[,]) = Array.init (length array) (fun idx -> array.[(idx/Array2D.length2 array),(idx%Array2D.length2 array)])

        // Stack two 'Array2D' columnwise.
        // For example, array1 (NxD) and array2(NxE) then produce an 'Array2D'(Nx(D+E))
        let stackColumn (array1:'T[,]) (array2:'T[,]) = 
            if (Array2D.length1 array1 = Array2D.length1 array2) then 
                let out = Array2D.zeroCreate (Array2D.length1 array1) (Array2D.length2 array1 + Array2D.length2 array2)
                out.[*,0..Array2D.length2 array1-1] <- array1
                out.[*,Array2D.length2 array1..] <- array2
                out
            else invalidArg "Array2D" "First dimension of first and second array2d doesn't match for columnwise stack."
          
        // Stack two 'Array2D' rowwise.
        // For example, array1 (NxD) and array2(ExD) then produce an 'Array2D'((N+E)xD)
        let stackRow (array1:'T[,]) (array2:'T[,]) = 
            if (Array2D.length2 array1 = Array2D.length2 array2) then 
                let out = Array2D.zeroCreate (Array2D.length1 array1 + Array2D.length1 array2) (Array2D.length2 array1)
                out.[0..Array2D.length1 array1-1,*] <- array1
                out.[Array2D.length1 array1..,*] <- array2
                out
            else invalidArg "Array2D" "Second dimension of first and second array2d doesn't match for rowwise stack."
            

    module State =  
        open FSharpPlus
        open FSharpPlus.Data

        // Transform an array of 'State Monad' to into a 'State Monad' with an array.
        // Note : the state monad MUST NOT modify the state. 
        let inline traverseBack (arrM:State<'a,'b>[]) = fst <!> (Array.mapFoldBack State.run arrM) <!> State.get



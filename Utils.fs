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

    module Array = 

        let rand = new System.Random()

        let private swapInPlace i j (array: 'T[]) = 
            let tmp = array.[i]
            array.[i] <- array.[j]
            array.[j] <- tmp

        let shuffleInPlace array = Array.iteri (fun i _ -> array |> swapInPlace i (rand.Next(i,array.Length))) array; array

    module Seq = 
        
        let randomIndices minValue maxValue = Array.init (maxValue-minValue) (fun i -> i+minValue) |> Array.shuffleInPlace |> Seq.ofArray

        let inline repeat count s = seq {
            for i=0 to count-1 do
                yield! s
        }
        

    module Array2D = 
        let length (array:'T[,]) = Array2D.length1 array * Array2D.length2 array
        let shape (array:'T[,]) = Array2D.length1 array, Array2D.length2 array
        let row idx (array:'T[,]) = array.[idx,*]
        let col idx (array:'T[,]) = array.[*,idx]

        let setRow idx (values:'T[]) (array:'T[,]) = 
            if values.Length = Array2D.length2 array then 
                Array2D.init (Array2D.length1 array) (Array2D.length2 array) (fun i j -> if i=idx then values.[j] else array.[i,j])
            else invalidArg (nameof values) "Lenght of new row doesn't match second dimension of 'Array2D'"

        let setColumn idx (values:'T[]) (array:'T[,]) = 
            if values.Length = Array2D.length1 array then 
                Array2D.init (Array2D.length1 array) (Array2D.length2 array) (fun i j -> if j=idx then values.[i] else array.[i,j])
            else invalidArg (nameof values) "Lenght of new row doesn't match first dimension of 'Array2D'"

        // Apply the given function to each Row/Column and concatenate the result in a new 'Array'.
        let collectByRow f (array:'T[,]) = Array.mapi (fun i _ -> f array.[i,*]) array.[*,0]
        let collectByCol f (array:'T[,]) = Array.mapi (fun i _ -> f array.[*,i]) array.[0,*]

        // ~> column-vector
        let singleton (array:'T[]) = Array2D.init array.Length 1 (fun i _ -> array.[i])

        let ofSingleArray (i,j) (array:'T[]) = 
            if array.Length % j = 0 && array.Length / j = i then 
                Array2D.init i j (fun m n -> array.[m*j+n])
            else invalidArg (nameof array) "Array cannot be casted into Array2D. The dimensions must exactly match."

        // Fst : outer array
        // Snd : inner array
        let ofArray (array:'T[][]) = 
            if (Array.concat >> Array.length) array % array.[0].Length = 0 then     
                Array2D.init array.Length array.[0].Length (fun i j -> array.[i].[j])
            else invalidArg "array" "Jagged array cannot be casted into Array2D. At least one inner array hasn't the right size."

        // Flatten the 'Array2D' by concatenating the rows into an 'Array'.
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
            
        let mapi2 (mapping: int -> int -> 'a -> 'b -> 'c) (arr1:'a[,]) (arr2:'b[,]) = 
            if (Array2D.length1 arr1 = Array2D.length1 arr2) && (Array2D.length2 arr1 = Array2D.length2 arr2) then 
                let out = Array2D.zeroCreate (Array2D.length1 arr1) (Array2D.length2 arr1)
                for i in 0..Array2D.length1 arr1-1 do 
                    for j in 0..Array2D.length2 arr1-1 do 
                        out.[i,j] <- mapping i j arr1.[i,j] arr2.[i,j]
                out     
            else invalidArg (nameof arr1) "'Array2D's don't have same dimensions"

        // Combine two 'Array2D's into a new one by applying the given function to each pair of elements.
        let map2 (mapping:'a -> 'b -> 'c) = mapi2 (fun _ _ -> mapping)
            

    module State =  
        open FSharpPlus
        open FSharpPlus.Data

        // Transform an array of 'State Monad' to into a 'State Monad' with an array.
        // Note : If the monad modify the state, the modifications won't carry through the array.
        let inline traverseBack (arrM:State<'a,'b>[]) = fst <!> (Array.mapFoldBack State.run arrM) <!> State.get

        // Transform an 'Array2D' of 'State Monad' into a 'State Monad with an 'Array2D'.
        // May be slow ! (boxing and unboxing in and out of 'Array' and 'Array2D'.) 
        // Could be Parallized. 
        let inline traverseBack2D (arr2M:State<'a,'b>[,]) = 
            Array2D.flatten arr2M |> traverseBack |> map (Array2D.ofSingleArray (Array2D.length1 arr2M, Array2D.length2 arr2M))

        // Transform an array of 'State Monad' to into a 'State Monad' with an array.
        // Note : If the monad modify the state, the modifications WILL carry through the array.
        let inline accumulate (arrM:State<'a,'b>[]) = monad {
            let out = Array.zeroCreate arrM.Length
            for i = 0 to arrM.Length-1 do
                let! r = arrM.[i]
                out.[i] <- r
            return out
        }

        module Seq = 
            let inline accumulate (arrM:seq<State<'a,'b>>) = arrM |> Array.ofSeq |> accumulate



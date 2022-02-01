﻿[<AutoOpen>]
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

    module Array2D = 
        let length (array:'T[,]) = Array2D.length1 array * Array2D.length2 array
        let getRow idx (array:'T[,]) = array.[idx,*]
        let setRow idx values (array:'T[,]) = array.[idx,*] <- values; array
        let getCol idx (array:'T[,]) = array.[*,idx]
        let setCol idx values (array:'T[,]) = array.[*,idx] <- values; array

        let ofSingleArray (array:'T[]) = Array2D.init array.Length 1 (fun i _ -> array.[i])

        let ofArray (array:'T[][]) = 
            if (Array.concat >> Array.length) array % array.[0].Length = 0 then     
                Array2D.init array.Length array.[0].Length (fun i j -> array.[i].[j])
            else invalidArg "array" "Jagged array cannot be casted into Array2D. At least one inner array hasn't the right size."

        let flatten (array:'T[,]) = Array.init (length array) (fun idx -> array.[(idx/Array2D.length2 array),(idx%Array2D.length2 array)])

        let areEqual arr1 arr2 = (flatten arr1) = (flatten arr2)
            

    module State =  
        open FSharpPlus
        open FSharpPlus.Data

        // Transform an array of 'State Monad' to into a 'State Monad' with an array.
        // Note : the state monad MUST NOT modify the state. 
        let inline traverseBack (arrM:State<'a,'b>[]) = fst <!> (Array.mapFoldBack State.run arrM) <!> State.get


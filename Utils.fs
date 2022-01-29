[<AutoOpen>]
module Utils 
    
    let mapTuple f (x,y) = (f x, f y)

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
        let setRow idx (array:'T[,]) values = array.[idx,*] <- values
        let getCol idx (array:'T[,]) = array.[*,idx]
        let setCol idx (array:'T[,]) values = array.[*,idx] <- values

        let ofArray (array: 'T[][]) = 
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


[<AutoOpen>]
module Utils 
    
    let mapTuple f (x,y) = (f x, f y)

    module Array2D = 
        let getRow idx (array:'T[,]) = array.[idx,*]
        let getCol idx (array:'T[,]) = array.[*,idx]

    module State =  
        open FSharpPlus
        open FSharpPlus.Data

        // Transform an array of 'State Monad' to into a 'State Monad' with an array.
        // Note : the state monad MUST NOT modify the state. 
        let inline traverseBack (arrM:State<'a,'b>[]) = fst <!> (Array.mapFoldBack State.run arrM) <!> State.get


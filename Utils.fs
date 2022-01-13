[<AutoOpen>]
module Utils 
    
    module Array2D = 
        // not used
        let inline cols idx (array2d:'T[,]) = array2d.[*,idx]


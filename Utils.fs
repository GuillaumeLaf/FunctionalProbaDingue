[<AutoOpen>]
module Utils 
    
    module Array2D = 
        let inline cols idx (array2d:'T[,]) = array2d.[*,idx]


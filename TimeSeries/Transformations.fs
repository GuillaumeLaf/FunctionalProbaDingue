namespace TimeSeries

open Monads

module Transformations = 

    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x
    let (>=>) g f = Monad.compose g f

    let inline differencedM (f:'a->'a) = 
        Option.map2 (fun current previous -> f current - f previous) 
                <!> Univariate.currentElementM ()
                <*> Univariate.elementAtLagM 1

    let differencedSeriesM = Univariate.mapM (differencedM id)
    let logDifferencedSeriesM = Univariate.mapM (differencedM log)
        
    let demeanM = 
        Option.map2 (fun current mean -> current - mean)
            <!> Univariate.currentElementM ()
            <*> Statistics.meanM
            |> Univariate.mapM
    
    let standardizeM = 
        Option.map2 (fun current std -> current / std)
            <!> Univariate.currentElementM ()
            <*> Statistics.stdM
            |> Univariate.mapM

    let normalizeM = Univariate.dataUpdating demeanM >>= (fun _ -> standardizeM) |> Univariate.stateKeeping

(*    let normalizeM =   
        let innerFunc (Univariate.State(idx,data,innovation)) = 
            let _, nxtState = Monad.run (Univariate.dataUpdating demeanM) (Univariate.State(idx,data,innovation))
            let result2, _ = Monad.run standardizeM nxtState
            result2, (Univariate.State(idx,data,innovation))
        Monad.M innerFunc*)
                             

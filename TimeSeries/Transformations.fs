namespace TimeSeries

open Monads

module Transformations = 

    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x
    let (>=>) g f = Monad.compose g f
    let (>=<) g f = Monad.chain g f

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
            |> Univariate.mapM // map over every element in 'data'.
    
    let standardizeM = 
        Option.map2 (fun current std -> current / std)
            <!> Univariate.currentElementM ()
            <*> Statistics.stdM
            |> Univariate.mapM

    // 'demeanM' must modify the state with the result of its computations,
    // if we then want to standardize the variance.
    // We finally comeback to the initial state by making it 'stateKeeping'.
    let normalizeM = (Univariate.dataUpdating demeanM >=< standardizeM) |> Univariate.stateKeeping
                             
    
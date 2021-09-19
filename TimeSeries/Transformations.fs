﻿namespace TimeSeries

open Monads

module Transformations = 

    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x
    let (>=>) g f = Monad.compose g f

    let inline differencedM (f:'a->'a) = 
        Option.map2 (fun current previous -> f current - f previous) 
                <!> Univariate.currentElementM
                <*> Univariate.elementAtLagM 1

    let differencedSeriesM = Univariate.mapM (differencedM id)
    let logDifferencedSeriesM = Univariate.mapM (differencedM log)
        
    let demeanM = 
        Option.map2 (fun current mean -> current - mean)
            <!> Univariate.currentElementM
            <*> Statistics.meanM
            |> Univariate.mapM
    
    let standardizeM = 
        Option.map2 (fun current std -> current / std)
            <!> Univariate.currentElementM
            <*> Statistics.stdM
            |> Univariate.mapM

    // This normalize function does not have the required form for a transformation monad.
    // It should not modify the input state (but here it does cause of 'dataUpdating').
    // I should find a way of remembering the input state. 
    let normalizeM = Univariate.dataUpdating demeanM >>= (fun _ -> standardizeM)
                             
namespace Model_Selection

open Timeseries
open TimeseriesType
open FSharpPlus

module Timeseries = 
    // Module including utility functions to treat 'TS' objects

    type FoldingType = Rolling | Fixed

    // 'Leave-P-Out' bounds computation
    // Output an 'Seq' of tuples : ((lowerBoundTrain,upperBoundTrain),(lowerBoundTest,upperBoundTest))
    // The sequence is walking forward by one each time.
    let rollingBoundsLPO (minLength:int) (p:int) = Seq.initInfinite (fun i -> (0,i+minLength-1),(i+minLength,i+minLength+p))
    let fixedBoundsLPO (minLength:int) (p:int) = Seq.initInfinite (fun i -> (i,i+minLength-1),(i+minLength,i+minLength+p))

    // 'Leave-One-Out'
    let rollingBoundsLOO = flip rollingBoundsLPO 0
    let fixedBoundsLOO = flip fixedBoundsLPO 0
       


    let crossValidationSplit = function
        | Fixed -> fixedSplit
        | Rolling -> rollingSplit
        
    

module CrossValidation = 
    // https://scikit-learn.org/stable/modules/cross_validation.html
    let x = 0
    


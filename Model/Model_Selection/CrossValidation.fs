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
    let rollingBoundsLPO (minLength:int) (overlap:int) (p:int) = Seq.initInfinite (fun i -> (0,i+minLength-1),(i+minLength-overlap,i+minLength+p-1))
    let fixedBoundsLPO (minLength:int) (overlap:int) (p:int) = Seq.initInfinite (fun i -> (i,i+minLength-1),(i+minLength-overlap,i+minLength+p-1))

    // 'Leave-One-Out'
    let rollingBoundsLOO minLength overlap = rollingBoundsLPO minLength overlap 1
    let fixedBoundsLOO minLength overlap = fixedBoundsLPO minLength overlap 1
       
    // Sequence of the relevant splits by ignoring some elements of the 'rollingBoundsLPO' sequence.
    let fixedSplit (foldLength:int) (overlap:int) = fixedBoundsLPO foldLength overlap foldLength
                                                        |> Seq.choose (fun x -> if (fst >> fst) x % foldLength = 0 then Some x else None)
        
    let rollingSplit (foldLength:int) (overlap:int) = rollingBoundsLPO foldLength overlap foldLength
                                                        |> Seq.choose (fun x -> if ((fst >> snd) x - foldLength+1) % foldLength = 0 then Some x else None)

    // Get the splitted timeseries
    let crossValidationSplit foldType kFold overlap ts = 
        let foldLength = ts.Length / kFold
        match foldType with
        | Fixed -> fixedSplit foldLength overlap
        | Rolling -> rollingSplit foldLength overlap
        |> Seq.map ( fun ((train1,train2),(test1,test2)) -> (TS.sub train1 train2 ts, TS.sub test1 test2 ts) )
        |> Seq.take (kFold-1)
        
    
module CrossValidation = 
    // https://scikit-learn.org/stable/modules/cross_validation.html
    let x = 0
    


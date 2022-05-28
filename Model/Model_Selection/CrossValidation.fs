namespace Model_Selection

open Timeseries
open TimeseriesType
open Models
open ModelType
open ModelState
open Model
open FSharpPlus
open FSharpPlus.Data

module CrossValidation = 
    // https://scikit-learn.org/stable/modules/cross_validation.html

    type FoldingType = Rolling | Fixed

    // 'Leave-P-Out' bounds computation
    // Output an 'Seq' of tuples : ((lowerBoundTrain,upperBoundTrain),(lowerBoundTest,upperBoundTest))
    // The sequence is walking forward by one each time.
    let _rollingBoundsLPO (minLength:int) (overlap:int) (p:int) = Seq.initInfinite (fun i -> (0,i+minLength-1),(i+minLength-overlap,i+minLength+p-1))
    let _fixedBoundsLPO (minLength:int) (overlap:int) (p:int) = Seq.initInfinite (fun i -> (i,i+minLength-1),(i+minLength-overlap,i+minLength+p-1))

    // 'Leave-One-Out'
    let _rollingBoundsLOO minLength overlap = _rollingBoundsLPO minLength overlap 1
    let _fixedBoundsLOO minLength overlap = _fixedBoundsLPO minLength overlap 1
       
    // Sequence of the relevant splits by ignoring some elements of the 'rollingBoundsLPO' sequence.
    let _fixedSplit (foldLength:int) (overlap:int) = _fixedBoundsLPO foldLength overlap foldLength
                                                        |> Seq.choose (fun x -> if (fst >> fst) x % foldLength = 0 then Some x else None)
        
    let _rollingSplit (foldLength:int) (overlap:int) = _rollingBoundsLPO foldLength overlap foldLength
                                                        |> Seq.choose (fun x -> if ((fst >> snd) x - foldLength+1) % foldLength = 0 then Some x else None)

    // Get the splitted timeseries
    let split foldType kFold overlap ts = 
        let foldLength = ts.Length / kFold
        match foldType with
        | Fixed -> _fixedSplit foldLength overlap
        | Rolling -> _rollingSplit foldLength overlap
        |> Seq.map ( fun ((train1,train2),(test1,test2)) -> (TS.sub train1 train2 ts, TS.sub test1 test2 ts) )
        |> Seq.take (kFold-1)
        
    
    // Compute the out-of-sample errors aggregated over the folds
    let errors foldType kFold optimizationFunc ts m = 
        let overlap = Model.maxLag m
        split foldType kFold overlap ts
            |> Seq.map (fun (trainTS,testTS) -> let fittedModel,_,_,_ = optimizationFunc m trainTS
                                                (ModelState.predictForAll fittedModel) </State.eval/> (Model.defaultState testTS fittedModel) 
                                                |> ((-) |> Option.map2 |> Array2D.map2) <| testTS.Data)
            |> Seq.map (Array2D.collectByRow (Array.skip overlap >> Stats.Computations.sum (flip ( ** ) 2f)))
            |> Seq.fold (Array.map2 (+)) ((Model.crossSection >> Array.zeroCreate) m)
    
        

    


namespace Models

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Math
open ComputationalGraph
open ComputationalGraph.GraphState
open Timeseries
open Timeseries.TimeseriesType
open Timeseries.TimeseriesState
open ModelType

module ModelState = 

    let graphToMonad = Array.map Graph.toMonad >> State.traverseBack 
    let graphToMonad2D = Array2D.map Graph.toMonad >> State.traverseBack2D

    // Get the Graph, Timeseries or Innovation State from the 'State' Model.
    let getGraphState (S(gs,_)) = gs                            : GraphType.S
    let getTimeseriesState (S(_,(idx,ts,_))) = (idx,ts)         : (int*TimeseriesType.TS)
    let getInnovationState (S(_,(idx,_,innov))) = (idx,innov)   : (int*TimeseriesType.TS)

    // Monad extension of 'getGraphState', 'getTimeSeriesState' and 'getInnovationState' functions
    let graphState = getGraphState <!> State.get                  : State<S, GraphType.S>
    let timeseriesState = getTimeseriesState <!> State.get        : State<S, (int*TimeseriesType.TS)>
    let innovationState = getInnovationState <!> State.get        : State<S, (int*TimeseriesType.TS)>

    // Evaluate a 'Graph Monad' or 'Timeseries Monad' with the corresponding state of the 'State Model'.
    let evalG (graphM:State<GraphType.S,'a>) = State.eval graphM <!> graphState
    let evalT (timeseriesM:State<(int*TimeseriesType.TS),'a>) = State.eval timeseriesM <!> timeseriesState
    let evalI (innovationM:State<(int*TimeseriesType.TS),'a>) = State.eval innovationM <!> innovationState

    // Evaluate a 'Graph Monad' or 'Timeseries Monad' which modify their corresponding state
    // with the corresponding state of the 'State Model'.
    let modifyG graphM = State.exec graphM <!> graphState >>= (fun newG -> State.modify (fun (S(_,oldT)) -> S(newG,oldT)))
    let modifyT timeseriesM = State.exec timeseriesM <!> timeseriesState >>= (fun (idx,ts) -> State.modify (fun (S(oldG,(_,_,innov))) -> S(oldG,(idx,ts,innov))))
    let modifyI innovationM = State.exec innovationM <!> innovationState >>= (fun (idx,innov) -> State.modify (fun (S(oldG,(_,ts,_))) -> S(oldG,(idx,ts,innov))))


    // Draw a random vector from 'rndVectorFunc' and update GraphState and TimeseriesState. 
    let updateInnovations rndVectorFunc = monad {
        let rndSampleVector = (rndVectorFunc >> Array2D.toOption) ()
        do! (GraphState.updateInnovations >> modifyG) rndSampleVector
        do! (TimeseriesState.setCurrentElements >> modifyI) rndSampleVector.[*,0] 
    }

    // Take a 'TS' monad with an 'Array2D' containing the updating rule for each variable.
    // Return a 'Model' monad with updated variables. 
    let updateVariables = evalT >> bind (GraphState.updateVariables >> modifyG) 

    let update rndVectorFunc (m:Model) = monad {
        do! updateVariables m.UpdateRule
        do! updateInnovations rndVectorFunc
    }

    // Model Monad to get a sample from the model
    let sample n rndVectorFunc (m:Model) = 
        monad {
            for i in 0..n-1 do 
                do! modifyT (TimeseriesState.setTime i) 
                do! update rndVectorFunc m
                let! currentResult = evalG m.GraphMonad
                do! modifyT (TimeseriesState.setCurrentElements currentResult)
        } 

    // Get the prediction for the model 'm' at the given 'idx' time.
    let predictFor (idx:int) (m:Model) = 
        monad {
            do! modifyT (TimeseriesState.setTime idx)
            do! updateVariables m.UpdateRule
            return! evalG m.GraphMonad
        }

    // Shorthand notation for predicting the first out-of-sample forecast value of the timeseries contained in 'm' Model.
    let predict m = evalT TimeseriesState.length >>= flip predictFor m

    // Get multistep prediction starting from 'idx' time for 'steps' steps.
    // Monad value contains the array of predictions.
    let multiPredictFor (idx:int) (steps:int) (m:Model) = 
        monad {
            let maxL = Model.maxLag m
            do! modifyT (TimeseriesState.setTime idx)

            let! lags = evalT (TimeseriesState.multipleLagElements maxL) 
            let! (tmpData:float32 option[,]) = flip Array2D.zeroCreate (maxL+steps) <!> (evalT size)
            tmpData.[*,0..maxL-1] <- lags
            do! modifyT (TimeseriesState.setData tmpData)

            let predictM i = 
                monad {
                    let! p = predictFor (i+maxL) m
                    do! modifyT (TimeseriesState.setCurrentElements p)
                    return p
                }
            
            let multiPredM = Array.init steps predictM |> (State.accumulate >> map (Array.transpose >> array2D))
            return! State.exec multiPredM <!> State.get
        } |> State.ignoreStateModif

    // Shorthand notation for multisteps out-of-sample predictions at the end of the current timeseries in 'm' Model.
    let multiPredict steps m = evalT TimeseriesState.length >>= (fun idx -> multiPredictFor idx steps m) 
















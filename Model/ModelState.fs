namespace Models

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Math
open ComputationalGraph
open ComputationalGraph.GraphType
open Timeseries
open Timeseries.TimeseriesType
open Timeseries.TimeseriesState
open ModelType

[<RequireQualifiedAccess>]
module ModelState = 

    let inline graphToMonad (gs:Graph<'T>[]) = Array.map Graph.toMonad >> State.traverseBack <| gs
    let inline graphToMonad2D (gs:Graph<'T>[,])= Array2D.map Graph.toMonad >> State.traverseBack2D <| gs

    // Get the Graph, Timeseries or Innovation State from the 'State' Model.
    let inline getGraphState (S(gs,_)) = gs                            : GraphType.S<'T>
    let inline getTimeseriesState (S(_,(idx,ts,_))) = (idx,ts)         : (int*TimeseriesType.TS< 'T >)
    let inline getInnovationState (S(_,(idx,_,innov))) = (idx,innov)   : (int*TimeseriesType.TS< 'T >)

    // Monad extension of 'getGraphState', 'getTimeSeriesState' and 'getInnovationState' functions
    let inline graphState () = getGraphState <!> State.get                  : State<S< 'T >, GraphType.S<'T>>
    let inline timeseriesState () = getTimeseriesState <!> State.get        : State<S< 'T >, (int*TimeseriesType.TS< 'T >)>
    let inline innovationState () = getInnovationState <!> State.get        : State<S< 'T >, (int*TimeseriesType.TS< 'T >)>

    // Evaluate a 'Graph Monad' or 'Timeseries Monad' with the corresponding state of the 'State Model'.
    let inline evalG (graphM:State<GraphType.S<'T>,'a>) = State.eval graphM <!> graphState()
    let inline evalT (timeseriesM:State<(int*TimeseriesType.TS< 'T >),'a>) = State.eval timeseriesM <!> timeseriesState()
    let inline evalI (innovationM:State<(int*TimeseriesType.TS< 'T >),'a>) = State.eval innovationM <!> innovationState()

    // Evaluate a 'Graph Monad' or 'Timeseries Monad' which modify their corresponding state
    // with the corresponding state of the 'State Model'.
    let inline modifyG graphM = State.exec graphM <!> graphState() >>= (fun newG -> State.modify (fun (S(_,oldT)) -> S(newG,oldT)))
    let inline modifyT timeseriesM = State.exec timeseriesM <!> timeseriesState() >>= (fun (idx,ts) -> State.modify (fun (S(oldG,(_,_,innov))) -> S(oldG,(idx,ts,innov))))
    let inline modifyI innovationM = State.exec innovationM <!> innovationState() >>= (fun (idx,innov) -> State.modify (fun (S(oldG,(_,ts,_))) -> S(oldG,(idx,ts,innov))))

    // Draw a random vector from 'rndVectorFunc' and update GraphState and TimeseriesState. 
    let inline updateInnovations rndVectorFunc = 
        monad {
            let rndSampleVector = rndVectorFunc ()
            do! (GraphState.updateInnovations >> modifyG) rndSampleVector
            do! (TimeseriesState.setCurrentElements >> modifyI) rndSampleVector.[*,0] 
        }

    // Take a 'TS' monad with an 'Array2D' containing the updating rule for each variable.
    // Return a 'Model' monad with updated variables. 
    let inline updateVariables m = evalT >> bind (GraphState.updateVariables >> modifyG) <| m

    let inline update rndVectorFunc (m:Model< 'T >) = monad {
        do! updateVariables m.UpdateRule
        do! updateInnovations rndVectorFunc
    }

    // Model Monad to get a sample from the model
    let inline sample n rndVectorFunc (m:Model< 'T >) = 
        monad {
            for i in 0..n-1 do 
                do! modifyT (TimeseriesState.setTime i) 
                do! update rndVectorFunc m
                let! currentResult = evalG m.GraphMonad
                do! modifyT (TimeseriesState.setCurrentElements currentResult)
        } 

    // Get the prediction for the model 'm' at the given 'idx' time.
    let inline predictFor (idx:int) (m:Model< 'T >) = 
        monad {
            do! modifyT (TimeseriesState.setTime idx)
            do! updateVariables m.UpdateRule
            return! evalG m.GraphMonad
        }

    // Shorthand notation for predicting the first out-of-sample forecast value of the timeseries contained in 'm' Model.
    let inline predict m = evalT (TimeseriesState.length()) >>= flip predictFor m

    let inline predictForArray (indices:int[]) (m:Model< 'T >) = Array.map (flip predictFor m) >> State.accumulate >> map (Array.transpose >> array2D) <| indices
    let inline predictForAll (m:Model< ^T >) = (flip Array.init id) <!> evalT (TimeseriesState.length()) >>= flip predictForArray m

    // Get multistep prediction starting from 'idx' time for 'steps' steps.
    // Monad value contains the array of predictions.
    let inline multiPredictFor (idx:int) (steps:int) (m:Model< 'T >) = 
        monad {
            let maxL = ModelOps.maxLag m
            do! modifyT (TimeseriesState.setTime idx)

            let! lags = evalT (TimeseriesState.multipleLagElements maxL) 
            let! (tmpData: ^T[,]) = flip Array2D.zeroCreate (maxL+steps) <!> (evalT (size()))
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
    let inline multiPredict steps m = evalT (TimeseriesState.length()) >>= (fun idx -> multiPredictFor idx steps m) 

    















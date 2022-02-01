namespace Models

open FSharpPlus
open FSharpPlus.Data
open ComputationalGraph
open ComputationalGraph.GraphState
open Timeseries
open Timeseries.TimeseriesState
open ModelType

module ModelState = 

    let graphToMonad = Array.map Graph.toMonad >> State.traverseBack 

    // Get the Graph, Timeseries or Innovation State from the 'State' Model.
    let getGraphState (S(gs,_)) = gs                            : GraphType.S
    let getTimeseriesState (S(_,(idx,ts,_))) = (idx,ts)         : (int*TimeseriesType.TS)
    let getInnovationState (S(_,(idx,_,innov))) = (idx,innov)   : (int*TimeseriesType.TS)

    // Monad extension of 'getGraphState', 'getTimeSeriesState' and 'getInnovationState' functions
    let graphState () = getGraphState <!> State.get                  : State<S, GraphType.S>
    let timeseriesState () = getTimeseriesState <!> State.get        : State<S, (int*TimeseriesType.TS)>
    let innovationState () = getInnovationState <!> State.get        : State<S, (int*TimeseriesType.TS)>

    // Evaluate a 'Graph Monad' or 'Timeseries Monad' with the corresponding state of the 'State Model'.
    let evalG (graphM:State<GraphType.S,'a>) = State.eval graphM <!> graphState()
    let evalT (timeseriesM:State<(int*TimeseriesType.TS),'a>) = State.eval timeseriesM <!> timeseriesState()
    let evalI (innovationM:State<(int*TimeseriesType.TS),'a>) = State.eval innovationM <!> innovationState()

    // Evaluate a 'Graph Monad' or 'Timeseries Monad' which modify their corresponding state
    // with the corresponding state of the 'State Model'.
    let modifyG graphM = State.exec graphM <!> graphState() >>= (fun newG -> State.modify (fun (S(_,oldT)) -> S(newG,oldT)))
    let modifyT timeseriesM = State.exec timeseriesM <!> timeseriesState() >>= (fun (idx,ts) -> State.modify (fun (S(oldG,(_,_,innov))) -> S(oldG,(idx,ts,innov))))
    let modifyI innovationM = State.exec innovationM <!> innovationState() >>= (fun (idx,innov) -> State.modify (fun (S(oldG,(_,ts,_))) -> S(oldG,(idx,ts,innov))))

    // Draw a random vector from 'rndVectorFunc' and update GraphState and TimeseriesState. 
    let updateInnovations rndVectorFunc = monad {
        let rndSampleVector = rndVectorFunc ()
        do! (GraphState.updateInnovations >> modifyG) rndSampleVector
        do! (TimeseriesState.setCurrentElements >> modifyI) rndSampleVector 
    }

    let updateVariables = evalT >> bind (GraphState.updateVariables >> modifyG) 

    let update rndVectorFunc (m:Model) = monad {
        do! updateVariables m.updateRule
        do! updateInnovations rndVectorFunc
    }

    // Model Monad to get a sample from the model
    let sample n rndVectorFunc (m:Model) = 
        monad {
            for i in 0..n-1 do 
                do! modifyT (TimeseriesState.setTime i) 
                do! update rndVectorFunc m
                let! currentResult = evalG m.graphMonad
                do! modifyT (TimeseriesState.setCurrentElements currentResult)
        } 






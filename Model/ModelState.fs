namespace Models

open FSharpPlus
open FSharpPlus.Data
open ComputationalGraph
open Timeseries
open ModelType

module ModelState = 

    // Get the Graph or Timeseries State from the 'State' Model.
    let getGraphState (S(gs,_)) = gs            : GraphType.S
    let getTimeSeriesState (S(_,tss)) = tss     : (int*TimeseriesType.TS)

    // Monad extension of 'getGraphState' and 'getTimeSeriesState' functions
    let graphState () = getGraphState <!> State.get                  : State<S, GraphType.S>
    let timeseriesState () = getTimeSeriesState <!> State.get        : State<S, (int*TimeseriesType.TS)>

    // Evaluate a 'Graph Monad' or 'Timeseries Monad' with the corresponding state of the 'State Model'.
    let evalG (graphM:State<GraphType.S,'a>) = State.eval graphM <!> graphState()
    let evalT (timeseriesM:State<(int*TimeseriesType.TS),'a>) = State.eval timeseriesM <!> timeseriesState()

    // Evaluate a 'Graph Monad' or 'Timeseries Monad' which modify their corresponding state
    // with the corresponding state of the 'State Model'.
    let modifyG graphM = State.exec graphM <!> graphState() >>= (fun newG -> State.modify (fun (S(_,oldT)) -> S(newG,oldT)))
    let modifyT timeseriesM = State.exec timeseriesM <!> timeseriesState() >>= (fun newT -> State.modify (fun (S(oldG,_)) -> S(oldG,newT)))



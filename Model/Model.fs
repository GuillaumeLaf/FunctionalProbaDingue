namespace Models

open FSharpPlus
open FSharpPlus.Data
open ComputationalGraph
open ComputationalGraph.Graph
open TimeSeries
open TimeSeries.Multivariate


module Univariate = 
    // This module will only serve as an interface for the multivariate case.
    // We can always represent a Univariate model from a Multivariate one.
    let x = 0

module Multivariate = 
    module Monad = 
        // Type for the 'State' Monad of a model.
        // Fst : state of the graph
        // Snd : state of the timeseries
        type S<'T> = S of Graph.Monad.S<'T> * (int*TimeSeries.Multivariate.TS)

        let getGraphState (S(gs,_)) = gs            : Graph.Monad.S<'T>
        let getTimeSeriesState (S(_,tss)) = tss     : (int*TimeSeries.Multivariate.TS)

        let graphState () = getGraphState <!> State.get                  : State<S<'T>, Graph.Monad.S<'T>>
        let timeseriesState () = getTimeSeriesState <!> State.get        : State<S<'T>, (int*TimeSeries.Multivariate.TS)>

        let evalG (graphM:State<Graph.Monad.S<'T>,'a>) = State.eval graphM <!> graphState()
        let evalT (timeseriesM:State<(int*TimeSeries.Multivariate.TS),'a>) = State.eval timeseriesM <!> timeseriesState()
        let modifyG graphM = State.exec graphM <!> graphState() >>= (fun newG -> State.modify (fun (S(_,oldT)) -> S(newG,oldT)))
        let modifyT timeseriesM = State.exec timeseriesM <!> timeseriesState() >>= (fun newT -> State.modify (fun (S(oldG,_)) -> S(oldG,newT)))

    type Model = 
        | VAR of nTS:int * order:int      // Vector Autoregressive Model

    type Model<'T>(m:Model) as self = 
        let model = m                                                                                : Model
        let graphs = self.get_Graph m                                                                : Graph[]
        let innovations = None                                                                       : TS option
        let _graphMonad = Array.map Graph.ToMonad graphs |> State.traverseBack                       : State<Graph.Monad.S<float32>,float32[]>
        let _updateRule = 
            match m with
            | VAR(_,order) -> [| for i in 1..order do TimeSeries.Multivariate.lagElementsDefault i |] |> (State.traverseBack >> map Array.transpose)
        
        let _updateM = Monad.evalT _updateRule >>= (Graph.Monad.updateVariables >> Monad.modifyG) 
        let _sample n = 0

        member this.fit() = 0
        member this.sample n = 0
        // Get the graph for a given model.
        // The graph must be multivariate in order to be able to sample simultaneously for all series.
        member this.get_Graph : (Model -> Graph[]) = function
            | VAR(nTS,order) -> Node.multivariateLinearCombinaison 0 (order-1) nTS |> Array.mapi (fun idxG g -> g + Input(Innovation(idxG,0)))
        
        






namespace Models

open FSharpPlus
open FSharpPlus.Data
open ComputationalGraph
open ComputationalGraph.Graph
open TimeSeries.Multivariate
open TimeSeries


module Univariate = 
    // This module will only serve as an interface for the multivariate case.
    // We can always represent a Univariate model from a Multivariate one.
    let x = 0

module Multivariate = 
    module Monad = 
        // Type for the 'State' Monad of a model.
        // Fst : state of the graph
        // Snd : state of the timeseries
        type S<'T> = S of Graph.Monad.S<'T> * (int*TimeSeries.Multivariate.TS<'T>)
    
    type Model = 
        | VAR of nTS:int * order:int      // Vector Autoregressive Model

    type Model<'T>(m:Model) as self = 
        let model = m                                                               : Model
        let graphs = self.get_Graph m                                               : Graph[]
        let graphMonad = Array.map Graph.ToMonad graphs |> State.traverseBack       : State<Graph.Monad.S<float32>,float32[]>
        member this.fit() = 0
        member this.sample n = 0

        // Get the graph for a given model.
        // The graph must be multivariate in order to be able to sample simultaneously for all series.
        member this.get_Graph : (Model -> Graph[]) = function
            | VAR(nTS,order) -> Node.multivariateLinearCombinaison 1 order nTS |> Array.mapi (fun idxG g -> g + Input(Innovation(idxG,0)))








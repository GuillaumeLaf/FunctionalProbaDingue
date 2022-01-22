namespace Models

open FSharpPlus
open FSharpPlus.Data
open ComputationalGraph
open ComputationalGraph.Graph
open TimeSeries


module Univariate = 
    // This module will only serve as an interface for the multivariate case.
    // We can always represent a Univariate model from a Multivariate one.
    let x = 0

module Multivariate = 
    let x = 0
    
    type Model = 
        | VAR of nTS:int * order:int      // Vector Autoregressive Model

    type Model<'T>(m:Model) as self = 
        let model = m                               : Model
        //let graph = self.get_Graph m                : Graph
        //let graphMonad = Graph.ToMonad graph        : State<Monad.S<float32>,float32>
        member this.fit() = 0
        member this.sample() = 0

        // Get the graph for a given model.
        // The graph must be multivariate in order to be able to sample simultaneously for all series.
(*        member this.get_Graph (m:Model) : Graph = 
            match m with
            | VAR(nTS,order) -> *)


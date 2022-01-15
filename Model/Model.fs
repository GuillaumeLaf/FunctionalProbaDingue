namespace Models

open FSharpPlus
open FSharpPlus.Data
open ComputationalGraph.Graph
open TimeSeries


module Univariate = 
    // This module will only serve as an interface for the multivariate case.
    // We can always represent a Univariate model from a Multivariate one.
    let x = 0

module Multivariate = 
    let x = 0
    
    type Model = 
        | VAR of orders:int[]      // Vector Autoregressive Model

    type Model<'T>(m:Model) = 
        let model = m                               : Model
        let graph = Model<'T>.get_Graph m               //: Graph<'T>
        let graphMonad = Graph<'T>.ToMonad graph        //: State<Monad.S<'T>,'T>
        member this.fit() = 0
        member this.sample() = 0

        static member inline get_Graph = function
            | VAR(orders) = 
        


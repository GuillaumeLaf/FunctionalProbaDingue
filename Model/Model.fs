namespace Models

open FSharpPlus
open FSharpPlus.Data
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Distributions
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

        // Get the Graph or Timeseries State from the 'State' Model.
        let getGraphState (S(gs,_)) = gs            : Graph.Monad.S<'T>
        let getTimeSeriesState (S(_,tss)) = tss     : (int*TimeSeries.Multivariate.TS)

        // Monad extension of 'getGraphState' and 'getTimeSeriesState' functions
        let graphState () = getGraphState <!> State.get                  : State<S<'T>, Graph.Monad.S<'T>>
        let timeseriesState () = getTimeSeriesState <!> State.get        : State<S<'T>, (int*TimeSeries.Multivariate.TS)>

        // Evaluate a 'Graph Monad' or 'Timeseries Monad' with the corresponding state of the 'State Model'.
        let evalG (graphM:State<Graph.Monad.S<'T>,'a>) = State.eval graphM <!> graphState()
        let evalT (timeseriesM:State<(int*TimeSeries.Multivariate.TS),'a>) = State.eval timeseriesM <!> timeseriesState()

        // Evaluate a 'Graph Monad' or 'Timeseries Monad' which modify their corresponding state
        // with the corresponding state of the 'State Model'.
        let modifyG graphM = State.exec graphM <!> graphState() >>= (fun newG -> State.modify (fun (S(_,oldT)) -> S(newG,oldT)))
        let modifyT timeseriesM = State.exec timeseriesM <!> timeseriesState() >>= (fun newT -> State.modify (fun (S(oldG,_)) -> S(oldG,newT)))

    // Model type with the parameters of the corresponding model.
    type Model = 
        | VAR of order:int      // Vector Autoregressive Model

    // 'Model' class
    type Model<'T>(nTimeseries:int, m:Model) = 
        let n = nTimeseries
        let model = m                                                                                

        // Compute the 'Array' of 'Graph's
        let g = match m with
                     | VAR(order) -> Node.multivariateLinearCombinaison 0 (order-1) nTimeseries 
                                        |> Array.mapi (fun idxG g -> g + Input(Innovation(idxG,0)))
        
        // 'Timeseries' object representing innovations for the model.
        let innovations = TimeSeries.Multivariate.TS.empty nTimeseries 
        
        // Graph Monad extension of the 'Graph'
        let graphMonad = Array.map Graph.ToMonad g |> State.traverseBack                            : State<Graph.Monad.S<float32>,float32[]>
        
        // Timeseries Monad for computing the new 'Variable' values in the 'State Graph' 
        let updateRule = 
            match m with
            | VAR(order) -> [| for i in 1..order do TimeSeries.Multivariate.lagElementsDefault i |] |> (State.traverseBack >> map Array.transpose)
        
        // Model Monad updating the 'Variable's in the 'State Model'.
        let updateVariables = Monad.evalT updateRule >>= (Graph.Monad.updateVariables >> Monad.modifyG) 

        // Sample an 'Array' from a multivariate normal
        // Note : Covariance matrix must be already initialized in the 'innovations' 'TS'.
        let randomNormalInnovations () = ( * ) (Matrix<float32>.Build.DenseOfArray(innovations.Stats.CholeskyLowerCovs))
                                               (Vector<float32>.Build.Random(nTimeseries, new Normal())) |> Vector.toArray

        let updateInnovations = Graph.Monad.updateInnovations (randomNormalInnovations ())

        let update = monad {
            do! updateVariables
            do! updateInnovations
        }

        // Model Monad to get a sample from the model
        let sample n = monad {
            for i in 0..n-1 do 
                do! Monad.modifyT (TimeSeries.Multivariate.setTime i) 
                do! update
                let! currentResult = Monad.evalG graphMonad
                do! Monad.modifyT (TimeSeries.Multivariate.setCurrentElements currentResult)

        } 
            

        member this.Graphs = g

        // Add the innovations covariance matrix. 
        member this.AddInnovationsCovariance covs = 
            if (Array2D.length1 covs) = nTimeseries && (Array2D.length2 covs) = nTimeseries then 
                innovations.Stats.AddCovs covs
            else invalidArg "Covs" "Covariance matrix is not squared or not the same size as the number of timeseries."

        member this.Fit() = 0
        member this.Sample n = 
            if innovations.Stats.HasCovs then 
                0
            else invalidArg "Innovations" "Innovations Covariance Matrix must be instantiated before sampling."
            
        
        






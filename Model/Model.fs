namespace Models

open FSharpPlus
open FSharpPlus.Data
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Distributions
open ComputationalGraph
open ComputationalGraph.Graph
open GraphType
open Timeseries

module Model = 

    module Univariate = 
        // This module will only serve as an interface for the multivariate case.
        // We can always represent a Univariate model from a Multivariate one.
        let x = 0

    module Multivariate = 

        // Model type with the parameters of the corresponding model.
        type Model = 
            | VAR of order:int      // Vector Autoregressive Model

        type ModelParameter = 
            | VARp of coeffs:float32[,]

        let defaultParameters = function        
            | VAR(order) -> VARp(Array2D.zeroCreate order order)

        let areParametersCompatible = function
            | VARp(p1),VARp(p2) -> (Array2D.length1 p1 = Array2D.length1 p2) && (Array2D.length2 p1 = Array2D.length2 p2)

        // 'Model' class
        type Model<'T>(nTimeseries:int, m:Model) = 
            let n = nTimeseries
            let model = m   
        
            // 'Timeseries' object representing innovations for the model.
            let mutable innovations = TimeSeries.Multivariate.TS.empty nTimeseries 

            // 'ModelParameter' storing parameter values of model.
            let mutable parameters = defaultParameters m

            // Compute the 'Array' of 'Graph's
            let g = match m with
                         | VAR(order) -> Node.multivariateLinearCombinaison 0 (order-1) nTimeseries 
                                            |> Array.mapi (fun idxG g -> g + Input(Innovation(idxG,0)))
        
            // Graph Monad extension of the 'Graph'
            let graphMonad = Array.map Graph.ToMonad g |> State.traverseBack                            : State<Graph.Monad.S<float32>,float32[]>
        
            // Timeseries Monad for computing the new 'Variable' values in the 'State Graph' 
            let updateRule = 
                match m with
                | VAR(order) -> [| for i in 1..order do TimeSeries.Multivariate.lagElementsDefault i |] 
                |> (State.traverseBack >> map Array.transpose >> map Array2D.ofArray)
        
            // Model Monad updating the 'Variable's in the 'State Model'.
            let updateVariables = Monad.evalT updateRule >>= (Graph.Monad.updateVariables >> Monad.modifyG) 

            // Sample an 'Array' from a multivariate normal
            // Note : Covariance matrix must be already initialized in the 'innovations' 'TS'.
            let randomNormalInnovations () = ( * ) (Matrix<float32>.Build.DenseOfArray(innovations.Stats.CholeskyLowerCovs))
                                                   (Vector<float32>.Build.Random(nTimeseries, new Normal())) |> Vector.toArray

            let updateInnovations idx = monad {
                let sampleInnov = randomNormalInnovations()
                do! Monad.modifyG (Graph.Monad.updateInnovations sampleInnov)
                innovations.SetAtTime idx sampleInnov
            }
        
            let updateAt idx = monad {
                do! updateVariables 
                do! updateInnovations idx
            }

            // Model Monad to get a sample from the model
            let sample n = 
                innovations <- TimeSeries.Multivariate.TS.init nTimeseries n
                monad {
                    for i in 0..n-1 do 
                        do! Monad.modifyT (TimeSeries.Multivariate.setTime i) 
                        do! updateAt i
                        let! currentResult = Monad.evalG graphMonad
                        do! Monad.modifyT (TimeSeries.Multivariate.setCurrentElements currentResult)
                } 
            
            member this.Graphs = g

            member this.hasParameters = 
                match (parameters, (defaultParameters m)) with
                | VARp(p1),VARp(p2) -> p1 = p2

            // Add the innovations covariance matrix. 
            member this.AddInnovationsCovariance covs = 
                if (Array2D.length1 covs) = nTimeseries && (Array2D.length2 covs) = nTimeseries then 
                    innovations.Stats.AddCovs covs
                else invalidArg "Covs" "Covariance matrix is not squared or not the same size as the number of timeseries."

            // Add parameters to the current model.
            member this.AddParameters p = 
                if areParametersCompatible(parameters,p) then
                    parameters <- p
                else invalidArg "Parameters" "Model parameters 'p' are not compatible with current model specifications."
            

            member this.Fit() = 0
            member this.Sample n = 
                if innovations.Stats.HasCovs && this.hasParameters then 
                    0
                else invalidArg "Innovations" "Innovations Covariance Matrix must be instantiated before sampling."
            
        
        






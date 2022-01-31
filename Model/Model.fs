namespace Models

open FSharpPlus
open FSharpPlus.Data
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Distributions
open ComputationalGraph
open ComputationalGraph.GraphType
open ComputationalGraph.Graph
open Timeseries
open Timeseries.TimeseriesState
open ModelType
open ModelState


module Model = 
    
    let ts m = m.ts |> Option.get
    let innovations m = m.innovations |> Option.get
    let model m = m.model 
    let graphs m = m.graphs

    module ModelTimeseries = 
        // Timeseries Monad for computing the new 'Variable' values in the 'State Graph' 
        let updateRule = (function
            | VAR(var) -> [| for i in 1..var.order do lagElementsDefault i |]) 
                            >> State.traverseBack >> map Array.transpose >> map Array2D.ofArray

    // Module grouping function for creating/managing graphs of a given model.
    module ModelGraph = 
        
        let create = function
        | VAR(var) -> Node.multivariateLinearCombinaison 0 (var.order-1) var.n 
                       |> Array.mapi (fun idxG g -> Graph.add g (Input(Innovation(idxG,0)))) 

        let toMonad = Array.map Graph.toMonad >> State.traverseBack 

        let updateVariables = ModelTimeseries.updateRule >> evalT >> bind (GraphState.updateVariables >> modifyG) 


    let create (m:T) = 
        let tmp = ModelGraph.create m
        { n=tmp.Length; ts=None; innovations=None; model=m; graphs=tmp }

    // Add the innovations covariance matrix.
    let addInnovationCovariance cov (m:Model) = 
        if (Array2D.length1 cov) = m.n && (Array2D.length2 cov) = m.n then 
             let tmpInnov = innovations m
             { m with innovations= Some{ tmpInnov with stats= tmpInnov.stats.AddCovs cov } }
        else invalidArg "Covs" "Covariance matrix is not squared or not the same size as the number of timeseries."

    let randomNormalVector length lowerCholesky = ( * ) (Matrix<float32>.Build.DenseOfArray(lowerCholesky))
                                                        (Vector<float32>.Build.Random(length, new Normal())) |> Vector.toArray

    // Sample an 'Array' from a multivariate normal
    // Note : Covariance matrix must be already initialized in the 'innovations' 'TS'.
    let randomNormalInnovations (m:Model) = randomNormalVector ((ts >> TS.size) m) ((innovations >> TS.stats >> Statistics.Multivariate.Stats.lowerCholesky) m)


    
            
(*    module Univariate = 
        // This module will only serve as an interface for the multivariate case.
        // We can always represent a Univariate model from a Multivariate one.
        let x = 0




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
            
        
        *)






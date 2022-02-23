﻿namespace Models

open FSharpPlus
open FSharpPlus.Data
open ComputationalGraph
open ComputationalGraph.GraphType
open Timeseries
open Timeseries.TimeseriesState
open Timeseries.TimeseriesType
open ModelType
open ModelState


module Model = 

    [<RequireQualifiedAccess>]
    module ModelTimeseries = 
        // Timeseries Monad for computing the new 'Variable' values in the 'State Graph' 
        let rec updateRule = 
            let rec loop g = 
                match g with
                | VAR(var) -> [| for i in 1..var.order do lagElementsDefault i |]
                | ErrorModel(inner) -> Array.append [|currentElementsDefault|] (loop inner)
            loop >> Utils.State.traverseBack >> map Array.transpose >> map Utils.Array2D.ofArray
            
    // Module grouping function for creating/managing graphs of a given model.
    [<RequireQualifiedAccess>]
    module ModelGraph = 
        
        // Build Graph
        let rec create = function
            | VAR(var) -> Node.multivariateLinearCombinaison 0 (var.order-1) var.n 
                           |> Array.mapi (fun idxG g -> Graph.add g (Input(Innovation(idxG,0)))) 
            | ErrorModel(inner) -> let err i = (Graph.shift Variable 1) >> (-) (Input(Variable(i,0)))
                                   let errSquared i g = err i g * err i g
                                   create inner |> Array.mapi errSquared

        let rec covariance = function
            | VAR(var) -> var.covariance 
            | ErrorModel(inner) -> covariance inner

        let cholesky = covariance >> Option.get >> Utils.cholesky

        let rec addCovariance cov = function
            | VAR(var) -> VAR({var with covariance=Some cov})
            | ErrorModel(inner) -> addCovariance cov inner
        
        

    let create (m:DGP) = 
        let tmp = ModelGraph.create m
        { N=tmp.Length; T=0; Ts=None; Innovations=None; Model=m; 
          Graphs=tmp; GraphMonad=ModelState.graphToMonad tmp; 
          GraphGradient=ModelState.graphToMonad2D (Graph.gradient tmp)
          UpdateRule=ModelTimeseries.updateRule m } 

    

    // Sample an 'Array' from a multivariate normal
    // Note : Covariance matrix must be already initialized in the 'innovations' 'TS'.
    let randomNormalInnovations (m:Model) () = Utils.randomNormalVector ((Model.ts >> Option.get >> TS.size) m) ((Model.innovations >> Option.get >> Stats.lowerCholeskyCov >> fst) m)
                                                 |> (Array.map Array.singleton >> Utils.Array2D.ofArray)

    let sample n (m:Model) = 
        if (ModelGraph.covariance m.Model) <> None then 
            let corrInnov = TS.addLowerCholeskyCov (ModelGraph.cholesky m.Model) (TS.zeroCreate m.N n)
            let reInitModel = { m with Ts=Some (TS.zeroCreate m.N n); Innovations=Some corrInnov } 

            let defaultState = defaultState reInitModel
            let newInnovFunc = randomNormalInnovations reInitModel
            let (S(_,(_,ts,innov))) = State.exec (ModelState.sample n newInnovFunc reInitModel) defaultState      
            { reInitModel with T=ts.Length; Ts=Some ts; Innovations=Some innov }
        else invalidArg "Innovations" "Innovations and its Covariance Matrix must be instantiated before sampling."

    // Fit the model with the given optimizer.
    // Model should already contain the data.
    let fit (m:Model) (opt:Optimisation.Optimizer) = 
        let innovations = TS.zeroCreate m.N m.T |> Some
        let errModel = create (ErrorModel(m.Model)) |> Model.setTs m.Ts |> Model.setInnovations innovations
        Optimisation.fit errModel opt
        
        





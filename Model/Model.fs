namespace Models

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
                | VAR(var) -> [| for i in 1..var.order do lagElements i |]
                | ErrorModel(inner,_) -> Array.append [|currentElements|] (loop inner)
            loop >> Utils.State.traverseBack >> map Array.transpose >> map Utils.Array2D.ofArray
            
    // Module grouping function for creating/managing graphs of a given model.
    [<RequireQualifiedAccess>]
    module ModelGraph = 

        let errorGraph errorType errorsG = 
            match errorType with
            | SquaredError -> Array.map (fun e -> Polynomial(e,2)) errorsG
            | L2Regu(lambda) -> let regu = Graph.collectUniqueParameters >> Array.map Input >> Node.Vector.createFrom >> Node.Vector.normL2
                                Array.map (fun e -> Polynomial(e,2) + Constant(lambda) * regu e) errorsG 
        
        // Build Graph
        // First element of array for first TS.
        let rec create = function
            | VAR(var) -> Node.multivariateLinearCombinaison 0 (var.order-1) var.n 
                           |> Array.mapi (fun idxG g -> Graph.add g (Input(Innovation(idxG,0)))) 
            | ErrorModel(inner,errType) -> let err i = (Graph.shift Variable 1) >> (-) (Input(Variable(i,0))) 
                                           (create >> Array.mapi err >> errorGraph errType) inner

        // Get 'Graph' state initiated with zeros
(*        let zeroGraphState dgp = GraphType.S(zeroParameters dgp |> Array2D.toOption, zeroVariables dgp |> Array2D.toOption, zeroInnovations dgp |> Array2D.toOption) 
        let defaultGraphState dgp = GraphType.S(parameters dgp, zeroVariables dgp |> Array2D.toOption, zeroInnovations dgp |> Array2D.toOption) *)

(*    let zeroCreate (m:Model) = ModelType.S(zeroGraphState m.Model, (0,Option.get m.Ts, Option.get m.Innovations)) *)
        
        
    let create (m:DGP) = 
        let tmp = ModelGraph.create m
        { Model=m; Graphs=tmp; GraphMonad=ModelState.graphToMonad tmp; 
        GraphGradient=ModelState.graphToMonad2D (Graph.gradient tmp);
        UpdateRule=ModelTimeseries.updateRule m } 

    // Sample an 'Array' from a multivariate normal
    let randomNormalInnovations cholesky N () = Utils.randomNormalVector N cholesky |> (Array.map Array.singleton >> array2D)

    let sample n (m:Model) = 
        if (Model.covariance m) <> None then 
            let ts = Array2D.zeroCreate (Model.crossSection m) n |> TS.create
            let innov = Array2D.zeroCreate (Model.crossSection m) n |> TS.create
            
            let defaultState = ModelType.S(ModelGraph.defaultGraphState m.Model, (0,ts, innov))
            let newInnovFunc = randomNormalInnovations (Model.cholesky m) (Model.crossSection m)

            let (S(_,(_,ts,innov))) = State.exec (ModelState.sample n newInnovFunc m) defaultState      
            m, ts, innov
        else invalidArg "Innovations" "Innovations and its Covariance Matrix must be instantiated before sampling."

    // Fit the model with the given optimizer.
    // Model should already contain the data.
    let fit (m:Model) (opt:Optimisation.Optimizer) (errorType:ErrorType) (ts:TS) = // Add 'TS' object containing the data -> check number of timeseries matched 'Model'-'DGP'
        let innovations = TS.zero_like ts
        let errModel = create (ErrorModel(m.Model,errorType)) 
        Optimisation.fit errModel opt
        
        





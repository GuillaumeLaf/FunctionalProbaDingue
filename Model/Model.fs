namespace Models

open FSharpPlus
open FSharpPlus.Data
open ComputationalGraph
open ComputationalGraph.GraphType
open Timeseries
open Timeseries.TimeseriesState
open Timeseries.TimeseriesType
open ModelType

[<RequireQualifiedAccess>]
module Model = 

    [<RequireQualifiedAccess>]
    module ModelTimeseries =

        // Timeseries Monad for computing the new 'Variable' values in the 'State Graph' 
        let rec inline updateRule (dgp:DGP<'T>) = 
            let rec loop (g:DGP<'T>) = 
                match g with
                | VAR(var) -> [| for i in 1..var.order do lagElements i |]
                | ErrorModel(inner,_) -> Array.append [|currentElements()|] (loop inner)
            loop >> Utils.State.traverseBack >> map Array.transpose >> map array2D <| dgp
            
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
        
    let inline create (m:DGP<'T>) = 
        let tmp = ModelGraph.create m
        { Model=m; Graphs=tmp; GraphMonad=ModelState.graphToMonad tmp; 
        GraphGradient=ModelState.graphToMonad2D (Graph.gradient tmp);
        UpdateRule=ModelTimeseries.updateRule m } 

    // Sample an 'Array' from a multivariate normal
    let randomNormalInnovations cholesky N () = Utils.randomNormalVector N cholesky |> (Array.map Array.singleton >> array2D)

    // Huge number of type constraints since using cholesky and random draws
    let inline sample n (m:Model<'T>) = 
        if (ModelOps.covariance m) <> None then 
            let ts = TS.zeroCreate (ModelOps.crossSection m) n 
            let defaultState = ModelOps.defaultState ts m

            let newInnovFunc = randomNormalInnovations (ModelOps.cholesky m) (ModelOps.crossSection m)

            let (S(_,(_,ts,innov))) = State.exec (ModelState.sample n newInnovFunc m) defaultState      
            m, ts, innov
        else invalidArg "Innovations" "Innovations and its Covariance Matrix must be instantiated before sampling."

    // Fit the model with the given optimizer.
    // Output the final fitted model and optimizer along with original 'TS' and in-sample errors 'TS'. 
    let inline fit (opt:Optimisation.Optimizer<'T>) (errorType:ErrorType<'T>) (epochs:int) (m:Model<'T>) (ts:TS<'T>) = 
        if ModelOps.crossSection m = TS.size ts then
            let errModel = create (ErrorModel(m.Model,errorType)) 
            let fittedParameters = Optimisation.fit errModel opt epochs ts
                                        |> (Optimisation.getModelState >> ModelState.getGraphState >> GraphState.getParameters) 
            printfn "%A" fittedParameters
            ModelOps.setParameters fittedParameters m, opt, errorType, ts
        else invalidArg "TS" "Timeseries cross-section dimension doesnt match given model cross-section dimension."
        
    let inline predict (m:Model<'T>) = flip ModelOps.defaultState m >> State.eval (ModelState.predict m) 
    let inline multiPredict (m:Model<'T>) (steps:int) = flip ModelOps.defaultState m >> State.eval (ModelState.multiPredict steps m) 
        
        
        





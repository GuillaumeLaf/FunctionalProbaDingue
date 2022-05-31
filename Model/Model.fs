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

    let inline model m = m.Model
    let inline graphs m = m.Graphs
    let inline graphMonad m = m.GraphMonad
    let inline updateRule m = m.UpdateRule

    let inline setModel x m = { m with Model=x }
    let inline setGraphMonad x m = { m with GraphMonad=x }
    let inline setUpdateRule x m = { m with UpdateRule=x }
    let inline setParameters p m = { m with Model=(Model< ^T >.model >> DGP.setParameters p) m }

    let inline parameterShape m = 
        let rec loop = function
            | VAR(var) -> var.n, var.n*var.order
            | ErrorModel(inner,_) -> loop inner
        loop m.Model

    let inline variableShape m = 
        let rec loop = function
            | VAR(var) -> var.n, var.order
            | ErrorModel(inner,_) -> loop inner |> (fun (i,j) -> (i,j+1))
        loop m.Model

    let inline innovationShape m = 
        let rec loop = function
            | VAR(var) -> var.n, 1
            | ErrorModel(inner,_) -> loop inner 
        loop m.Model

    let inline crossSection m = 
        let rec loop = function
            | VAR(var) -> var.n
            | ErrorModel(inner,_) -> loop inner
        loop m.Model

    let inline maxLag m = 
        let rec loop = function
            | VAR(var) -> var.order
            | ErrorModel(inner,_) -> loop inner
        loop m.Model

    // Unsafe unboxing -> parameters should be already be defined in 'DGP'
    let inline parameters m =
        let rec loop = function
            | VAR(var) -> var.parameters |> Option.get |> Array.reduce Utils.Array2D.stackColumn
            | ErrorModel(inner,_) -> loop inner
        loop m.Model

    let inline defaultParameters m = (Model< ^T >.parameterShape >> (fun (i,j) -> Array2D.create i j (Some 0f))) m
    let inline defaultVariables m = (Model< ^T >.variableShape >> (fun (i,j) -> Array2D.create i j (Some 0f))) m
    let inline defaultInnovations m = (Model< ^T >.innovationShape >> (fun (i,j) -> Array2D.create i j (Some 0f))) m

    // Covariance between innovations
    let inline covariance m =
        let rec loop = function
            | VAR(var) -> var.covariance 
            | ErrorModel(inner,_) -> loop inner
        loop m.Model

    let inline setCovariance cov m = 
        let rec loop = function
            | VAR(var) -> VAR({var with covariance=Some cov})
            | ErrorModel(inner,_) -> loop inner
        loop m.Model |> flip Model< ^T >.setModel m

    let inline cholesky m = (Model< ^T >.covariance >> Option.get >> Utils.cholesky) m

    // State Innovations are always initiated to zero. 
    let inline defaultState ts m = S(GraphType.S(Model< ^T >.parameters m, Model< ^T >.defaultVariables m, Model< ^T >.defaultInnovations m), (0,ts, TS.zero_like ts))

    // Everything is set to zero (even parameters).
    let inline defaultEmptyState ts m = S(GraphType.S(Model< ^T >.defaultParameters m, Model< ^T >.defaultVariables m, Model< ^T >.defaultInnovations m), (0,ts, TS.zero_like ts))



    [<RequireQualifiedAccess>]
    module ModelTimeseries =

        // Timeseries Monad for computing the new 'Variable' values in the 'State Graph' 
        let rec updateRule = 
            let rec loop g = 
                match g with
                | VAR(var) -> [| for i in 1..var.order do lagElements i |]
                | ErrorModel(inner,_) -> Array.append [|currentElements<float32 option>|] (loop inner)
            loop >> Utils.State.traverseBack >> map Array.transpose >> map array2D
            
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
        
    let create (m:DGP) = 
        let tmp = ModelGraph.create m
        { Model=m; Graphs=tmp; GraphMonad=ModelState.graphToMonad tmp; 
        GraphGradient=ModelState.graphToMonad2D (Graph.gradient tmp);
        UpdateRule=ModelTimeseries.updateRule m } 

    // Sample an 'Array' from a multivariate normal
    let randomNormalInnovations cholesky N () = Utils.randomNormalVector N cholesky |> (Array.map Array.singleton >> array2D)

    let sample n (m:Model) = 
        if (Model.covariance m) <> None then 
            let ts = TS.zeroCreateOption (Model.crossSection m) n 
            let defaultState = Model.defaultState ts m

            let newInnovFunc = randomNormalInnovations (Model.cholesky m) (Model.crossSection m)

            let (S(_,(_,ts,innov))) = State.exec (ModelState.sample n newInnovFunc m) defaultState      
            m, ts, innov
        else invalidArg "Innovations" "Innovations and its Covariance Matrix must be instantiated before sampling."

    // Fit the model with the given optimizer.
    // Output the final fitted model and optimizer along with original 'TS' and in-sample errors 'TS'. 
    let fit (opt:Optimisation.Optimizer) (errorType:ErrorType) (epochs:int) (m:Model) (ts:TS<float32 option>) = 
        if Model.crossSection m = TS<float32 option>.size ts then
            let errModel = create (ErrorModel(m.Model,errorType)) 
            let fittedParameters = Optimisation.fit errModel opt epochs ts
                                        |> (Optimisation.getModelState >> ModelState.getGraphState >> GraphState.getParameters) 
            Model.setParameters fittedParameters m, opt, errorType, ts
        else invalidArg "TS" "Timeseries cross-section dimension doesnt match given model cross-section dimension."
        
    let predict (m:Model) = flip Model.defaultState m >> State.eval (ModelState.predict m) 
    let multiPredict (m:Model) (steps:int) = flip Model.defaultState m >> State.eval (ModelState.multiPredict steps m) 
        
        
        





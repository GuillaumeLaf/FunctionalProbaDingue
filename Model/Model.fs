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
            | ErrorModel(inner) -> create inner |> Array.mapi (fun i -> (Graph.shift Variable 1) >> (-) (Input(Variable(i,0))))

        let rec parameters = function
            | VAR(var) -> var.parameters |> Option.get |> Array.reduce Utils.Array2D.stackColumn
            | ErrorModel(inner) -> parameters inner

        let rec defaultVariables = function
            | VAR(var) -> Array2D.zeroCreate var.n var.order
            | ErrorModel(inner) -> let tmp = defaultVariables inner
                                   Array2D.zeroCreate (Array2D.length1 tmp) (Array2D.length2 tmp + 1)

        let rec defaultInnovations = function
            | VAR(var) -> Array2D.zeroCreate var.n 1
            | ErrorModel(inner) -> defaultInnovations inner

        let rec covariance = function
            | VAR(var) -> var.covariance 
            | ErrorModel(inner) -> covariance inner

        let cholesky = covariance >> Option.get >> Utils.cholesky

        let rec addCovariance cov = function
            | VAR(var) -> VAR({var with covariance=Some cov})
            | ErrorModel(inner) -> addCovariance cov inner
        
        let state dgp = GraphType.S(parameters dgp, defaultVariables dgp, defaultInnovations dgp) 

    let create (m:DGP) = 
        let tmp = ModelGraph.create m
        { N=tmp.Length; Ts=None; Innovations=None; Model=m; 
          Graphs=tmp; GraphMonad=ModelState.graphToMonad tmp; 
          UpdateRule=ModelTimeseries.updateRule m } 

    let defaultState (m:Model) = ModelType.S(ModelGraph.state m.Model, (0,Option.get m.Ts, Option.get m.Innovations))

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
            { reInitModel with Ts=Some ts; Innovations=Some innov }
        else invalidArg "Innovations" "Innovations and its Covariance Matrix must be instantiated before sampling."

    // Fit the model with the given optimizer.
    // Model should already contain the data.
    let fit (m:Model) (opt:Optimisation.Optimizer) = 
        let errModel = create (ErrorModel(m.Model)) |> Model.setTs m.Ts
        let optimized = Optimisation.fit errModel opt
        





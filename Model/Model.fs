namespace Models

open FSharpPlus
open FSharpPlus.Data
open ComputationalGraph
open ComputationalGraph.GraphType
open ComputationalGraph.Graph
open Timeseries
open Timeseries.TimeseriesState
open Timeseries.TimeseriesType
open ModelType
open ModelState


module Model = 

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

        let parameters = function
            | VAR(var) -> var.parameters |> Option.get |> Array.reduce Array2D.stackColumn

        let defaultVariables = function
            | VAR(var) -> Array2D.zeroCreate var.n var.order

        let defaultInnovations = function
            | VAR(var) -> Array2D.zeroCreate var.n 1

        let covariance = function
            | VAR(var) -> var.covariance |> Option.get

        let addCovariance cov = function
            | VAR(var) -> VAR({var with covariance=Some cov})
        
        let state dgp = GraphType.S(parameters dgp, defaultVariables dgp, defaultInnovations dgp) 

    let create (m:DGP) = 
        let tmp = ModelGraph.create m
        { N=tmp.Length; Ts=None; Innovations=None; Model=m; 
          Graphs=tmp; GraphMonad=ModelState.graphToMonad tmp; UpdateRule=ModelTimeseries.updateRule m}

    let defaultState (m:Model) = S(ModelGraph.state m.Model, (0,Option.get m.Ts, Option.get m.Innovations))
        
    // Add the innovations covariance matrix to 'DGP' of 'Model' 'm'
(*    let addInnovationCovariance cov (m:Model) = 
        if (Array2D.length1 cov) = m.n && (Array2D.length2 cov) = m.n then 
            { m with model=ModelGraph.addCovariance cov m.model }
        else invalidArg "Covs" "Covariance matrix is not squared or not the same size as the number of timeseries."*)

    // Sample an 'Array' from a multivariate normal
    // Note : Covariance matrix must be already initialized in the 'innovations' 'TS'.
    let randomNormalInnovations (m:Model) () = randomNormalVector ((Model.ts >> Option.get >> TS.size) m) ((Model.innovations >> Option.get >> Stats.lowerCholeskyCov >> fst) m)
                                                 |> (Array.map Array.singleton >> Array2D.ofArray)

    let sample n (m:Model) = 
        if true then 
            let newInnov = TS.create m.N n
            let corrInnov = ((TS.stats >> Stats.setLowerCholeskyCov ((ModelGraph.covariance >> cholesky) m.Model)) newInnov |> TS.setStats) newInnov
            let reInitModel = { m with Ts=Some (TS.create m.N n); Innovations=Some corrInnov } 

            let defaultState = defaultState reInitModel
            let newInnovFunc = randomNormalInnovations reInitModel
            let (S(_,(_,ts,innov))) = State.exec (ModelState.sample n newInnovFunc reInitModel) defaultState
            { reInitModel with Ts=Some ts; Innovations=Some innov }
        else invalidArg "Innovations" "Innovations and its Covariance Matrix must be instantiated before sampling."
    
    // Adapt how we compute contemporaneously correlation innovations from Innvovation 'TS' to record type 'DGP'.




(*    
            member this.Fit() = 0
          
        *)






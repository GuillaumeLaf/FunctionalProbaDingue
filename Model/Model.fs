namespace Models

open FSharpPlus
open FSharpPlus.Data
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
        { n=tmp.Length; ts=None; innovations=None; model=m; 
        graphs=tmp; graphMonad=ModelState.graphToMonad tmp; updateRule=ModelTimeseries.updateRule m}

    let defaultState (m:Model) = S(ModelGraph.state m.model, (0,ts m, innovations m))
        
    // Add the innovations covariance matrix to 'DGP' of 'Model' 'm'
(*    let addInnovationCovariance cov (m:Model) = 
        if (Array2D.length1 cov) = m.n && (Array2D.length2 cov) = m.n then 
            { m with model=ModelGraph.addCovariance cov m.model }
        else invalidArg "Covs" "Covariance matrix is not squared or not the same size as the number of timeseries."*)

    // Sample an 'Array' from a multivariate normal
    // Note : Covariance matrix must be already initialized in the 'innovations' 'TS'.
(*    let randomNormalInnovations2 (m:Model) () = randomNormalVector ((ts >> TS.size) m) ((innovations >> TS.stats >> Statistics.Multivariate.Stats.lowerCholesky) m)
                                                 |> (Array.map Array.singleton >> Array2D.ofArray)*)

    let randomNormalInnovations (m:Model) () = randomNormalVector ((ts >> TS.size) m) ((ModelGraph.covariance >> cholesky) m.model)
                                                |> (Array.map Array.singleton >> Array2D.ofArray)

    let sample n (m:Model) = 
        if true then 
            let newTS = TS.multivariateZeroCreate m.n n
            let newInnov = TS.multivariateZeroCreate m.n n
            let reInitModel = { m with ts=Some newTS; innovations=Some newInnov } 

            let defaultState = defaultState reInitModel
            let newInnovFunc = randomNormalInnovations reInitModel
            let (S(_,(_,ts,innov))) = State.exec (ModelState.sample n newInnovFunc reInitModel) defaultState
            { reInitModel with ts=Some ts; innovations=Some innov }
        else invalidArg "Innovations" "Innovations and its Covariance Matrix must be instantiated before sampling."
    
    // Adapt how we compute contemporaneously correlation innovations from Innvovation 'TS' to record type 'DGP'.




(*    
            member this.Fit() = 0
          
        *)






namespace Models

open FSharpPlus
open FSharpPlus.Data
open ComputationalGraph
open Timeseries
open TimeseriesType
open GraphType

module ModelType = 
    
    // Type for the 'State' Monad of a model.
    // Fst : state of the graph
    // Snd : state of the timeseries
    type S = S of GraphType.S * (int*TimeseriesType.TS<float32 option>*TimeseriesType.TS<float32 option>)

    // Vector Autoregressive Model
    // 'parameters' -> Fst : which timeseries
    //              -> Snd : which parameter
    //              -> Third : which lag
    type VAR = 
        {n:int; order:int; parameters:float32 option[,][] option; covariance:float32[,] option }

        // Convert parameters from shape in GraphState to correct shape in 'VAR'.
        static member convertParameters (p:float32 option[,]) var = Array.init var.order (fun i -> p.[*,(var.n*i)..(var.n*(i+1)-1)])
        static member setParameters p var = { var with parameters= VAR.convertParameters p var |> Some }

    type ErrorType = 
        | SquaredError
        | L2Regu of lambda:float32

    // Discriminated Union for grouping model types.
    // Contains the hyperparameters -> important for model selection
    type DGP = 
        | VAR of VAR 
        | ErrorModel of DGP * ErrorType

        static member setParameters p = function
            | VAR(var) -> VAR.setParameters p var |> VAR
            | ErrorModel(inner,errType) -> (DGP.setParameters p inner, errType) |> ErrorModel

    // Record Type representing a model.
    // Contains all information required for using a model.
    type Model = 
        { Model:DGP;
          Graphs:Graph[];
          GraphMonad:State<GraphType.S, float32 option[]>;
          GraphGradient:State<GraphType.S, float32 option[,]>;
          UpdateRule:State<(int*TimeseriesType.TS<float32 option>), float32 option[,]>;
        }
          
        static member inline model m = m.Model
        static member inline graphs m = m.Graphs
        static member inline graphMonad m = m.GraphMonad
        static member inline updateRule m = m.UpdateRule

        static member inline setModel x m = { m with Model=x }
        static member inline setGraphMonad x m = { m with GraphMonad=x }
        static member inline setUpdateRule x m = { m with UpdateRule=x }
        static member setParameters p m = { m with Model=(Model.model >> DGP.setParameters p) m }

        static member parameterShape m = 
            let rec loop = function
                | VAR(var) -> var.n, var.n*var.order
                | ErrorModel(inner,_) -> loop inner
            loop m.Model

        static member variableShape m = 
            let rec loop = function
                | VAR(var) -> var.n, var.order
                | ErrorModel(inner,_) -> loop inner |> (fun (i,j) -> (i,j+1))
            loop m.Model

        static member innovationShape m = 
            let rec loop = function
                | VAR(var) -> var.n, 1
                | ErrorModel(inner,_) -> loop inner 
            loop m.Model

        static member crossSection m = 
            let rec loop = function
                | VAR(var) -> var.n
                | ErrorModel(inner,_) -> loop inner
            loop m.Model

        static member maxLag m = 
            let rec loop = function
                | VAR(var) -> var.order
                | ErrorModel(inner,_) -> loop inner
            loop m.Model

        // Unsafe unboxing -> parameters should be already be defined in 'DGP'
        static member parameters m =
            let rec loop = function
                | VAR(var) -> var.parameters |> Option.get |> Array.reduce Utils.Array2D.stackColumn
                | ErrorModel(inner,_) -> loop inner
            loop m.Model

        static member defaultParameters m = (Model.parameterShape >> (fun (i,j) -> Array2D.create i j (Some 0f))) m
        static member defaultVariables m = (Model.variableShape >> (fun (i,j) -> Array2D.create i j (Some 0f))) m
        static member defaultInnovations m = (Model.innovationShape >> (fun (i,j) -> Array2D.create i j (Some 0f))) m

        // Covariance between innovations
        static member covariance m =
            let rec loop = function
                | VAR(var) -> var.covariance 
                | ErrorModel(inner,_) -> loop inner
            loop m.Model

        static member setCovariance cov m = 
            let rec loop = function
                | VAR(var) -> VAR({var with covariance=Some cov})
                | ErrorModel(inner,_) -> loop inner
            loop m.Model |> flip Model.setModel m

        static member cholesky m = (Model.covariance >> Option.get >> Utils.cholesky) m

        // State Innovations are always initiated to zero. 
        static member defaultState ts m = S(GraphType.S(Model.parameters m, Model.defaultVariables m, Model.defaultInnovations m), (0,ts, TS.zero_like ts))

        // Everything is set to zero (even parameters).
        static member defaultEmptyState ts m = S(GraphType.S(Model.defaultParameters m, Model.defaultVariables m, Model.defaultInnovations m), (0,ts, TS.zero_like ts))












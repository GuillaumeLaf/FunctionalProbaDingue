namespace Models

open FSharpPlus
open FSharpPlus.Data
open ComputationalGraph
open Timeseries
open Timeseries.TimeseriesType
open Models.ModelType

[<RequireQualifiedAccess>]
module ModelOps = 
    
    let inline model (m:Model<'T>) = m.Model
    let inline graphs (m:Model<'T>) = m.Graphs
    let inline graphMonad (m:Model<'T>) = m.GraphMonad
    let inline updateRule (m:Model<'T>) = m.UpdateRule

    let inline setModel x (m:Model<'T>) = { m with Model=x }
    let inline setGraphMonad x (m:Model<'T>) = { m with GraphMonad=x }
    let inline setUpdateRule x (m:Model<'T>) = { m with UpdateRule=x }
    let inline setParameters p (m:Model<'T>) = { m with Model=(model >> DGP.setParameters p) m }

    let inline parameterShape (m:Model<'T>) = 
        let rec loop = function
            | VAR(var) -> var.n, var.n*var.order
            | ErrorModel(inner,_) -> loop inner
        loop m.Model

    let inline variableShape (m:Model<'T>) = 
        let rec loop = function
            | VAR(var) -> var.n, var.order
            | ErrorModel(inner,_) -> loop inner |> (fun (i,j) -> (i,j+1))
        loop m.Model

    let inline innovationShape (m:Model<'T>) = 
        let rec loop = function
            | VAR(var) -> var.n, 1
            | ErrorModel(inner,_) -> loop inner 
        loop m.Model

    let inline crossSection (m:Model<'T>) = 
        let rec loop = function
            | VAR(var) -> var.n
            | ErrorModel(inner,_) -> loop inner
        loop m.Model

    let inline maxLag (m:Model<'T>) = 
        let rec loop = function
            | VAR(var) -> var.order
            | ErrorModel(inner,_) -> loop inner
        loop m.Model

    // Unsafe unboxing -> parameters should be already be defined in 'DGP'
    let inline parameters (m:Model<'T>) =
        let rec loop = function
            | VAR(var) -> var.parameters |> Option.get |> Array.reduce Utils.Array2D.stackColumn
            | ErrorModel(inner,_) -> loop inner
        loop m.Model

    let inline defaultParameters (m:Model<'T>) = (parameterShape >> (fun (i,j) -> Array2D.create<'T> i j LanguagePrimitives.GenericZero)) m
    let inline defaultVariables (m:Model<'T>) = (variableShape >> (fun (i,j) -> Array2D.create<'T> i j LanguagePrimitives.GenericZero)) m
    let inline defaultInnovations (m:Model<'T>) = (innovationShape >> (fun (i,j) -> Array2D.create<'T> i j LanguagePrimitives.GenericZero)) m

    // Covariance between innovations
    let inline covariance (m:Model<'T>) =
        let rec loop = function
            | VAR(var) -> var.covariance 
            | ErrorModel(inner,_) -> loop inner
        loop m.Model

    let inline setCovariance cov (m:Model<'T>) = 
        let rec loop = function
            | VAR(var) -> VAR({var with covariance=Some cov})
            | ErrorModel(inner,_) -> loop inner
        loop m.Model |> flip setModel m

    let inline cholesky (m:Model<'T>) = (covariance >> Option.get >> Utils.cholesky) m

    // State Innovations are always initiated to zero. 
    let inline defaultState (ts:TS<'T>) (m:Model<'T>) = S(GraphType.S(parameters m, defaultVariables m, defaultInnovations m), (0,ts, TS.zero_like ts))

    // Everything is set to zero (even parameters).
    let inline defaultEmptyState (ts:TS<'T>) (m:Model<'T>) = S(GraphType.S(defaultParameters m, defaultVariables m, defaultInnovations m), (0,ts, TS.zero_like ts))



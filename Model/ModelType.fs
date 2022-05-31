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
    type S< 'T when 'T : (static member Zero : 'T) > = S of GraphType.S<'T> * (int*TimeseriesType.TS<'T>*TimeseriesType.TS<'T>)

    // Vector Autoregressive Model
    // 'parameters' -> Fst : which timeseries
    //              -> Snd : which parameter
    //              -> Third : which lag
    type VAR<'T> = 
        {n:int; order:int; parameters:'T[,][] option; covariance:'T[,] option }

    type ErrorType<'T> = 
        | SquaredError
        | L2Regu of lambda:'T

    // Discriminated Union for grouping model types.
    // Contains the hyperparameters -> important for model selection
    type DGP<'T> = 
        | VAR of VAR<'T> 
        | ErrorModel of DGP<'T> * ErrorType<'T>

    // Record Type representing a model.
    // Contains all information required for using a model.
    type Model< 'T when 'T : (static member Zero : 'T) > = 
        { Model:DGP<'T>;
          Graphs:Graph<'T>[];
          GraphMonad:State<GraphType.S<'T>, 'T[]>;
          GraphGradient:State<GraphType.S<'T>, 'T[,]>;
          UpdateRule:State<(int*TimeseriesType.TS<'T>), 'T[,]>;
        }



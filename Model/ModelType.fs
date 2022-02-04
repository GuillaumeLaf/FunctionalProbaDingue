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
    type S = S of GraphType.S * (int*TimeseriesType.TS*TimeseriesType.TS)

    // Vector Autoregressive Model
    // 'parameters' -> Fst : which timeseries
    //              -> Snd : which parameter
    //              -> Third : which lag
    type VAR = {n:int; order:int; parameters:float32[,][] option; covariance:float32[,] option }

    type DGP = 
        | VAR of VAR 

    type Model = { n:int;
                   ts:TS option ;
                   innovations:TS option;
                   model:DGP;
                   graphs:Graph[];
                   graphMonad:State<GraphType.S, float32[]>
                   updateRule:State<(int*TimeseriesType.TS), float32[,]>
                   }


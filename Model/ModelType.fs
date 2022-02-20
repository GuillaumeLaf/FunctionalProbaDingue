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

    // Discriminated Union for grouping model types.
    type DGP = 
        | VAR of VAR 
        | ErrorModel of DGP

    // Record Type representing a model.
    // Contains all information required for using a model.
    type Model = 
        { N:int;
          Ts:TS option ;
          Innovations:TS option;
          Model:DGP;
          Graphs:Graph[];
          GraphMonad:State<GraphType.S, float32[]>
          UpdateRule:State<(int*TimeseriesType.TS), float32[,]>
          }

        static member n m = m.n
        static member ts m = m.Ts
        static member innovations m = m.Innovations
        static member model m = m.Model
        static member graphs m = m.Graphs
        static member graphMonad m = m.GraphMonad
        static member updateRule m = m.UpdateRule

        static member setN x m = { m with N=x }
        static member setTs x m = { m with Ts=x }
        static member setInnovations x m = { m with Innovations=x }
        static member setModel x m = { m with Model=x }
        static member setGraphMonad x m = { m with GraphMonad=x }
        static member setUpdateRule x m = { m with UpdateRule=x }
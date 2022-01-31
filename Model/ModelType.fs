namespace Models

open ComputationalGraph
open Timeseries
open TimeseriesType
open GraphType

module ModelType = 
    
    // Type for the 'State' Monad of a model.
    // Fst : state of the graph
    // Snd : state of the timeseries
    type S = S of GraphType.S * (int*TimeseriesType.TS)

    // Vector Autoregressive Model
    type VAR = {n:int; order:int; parameters:float32[,][] option }

    type T = 
        | VAR of VAR 

    type Model = { n:int;
                   ts:TS option ;
                   innovations:TS option;
                   model:T;
                   graphs:Graph[];
                   }


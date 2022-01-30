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

    type VAR = {n:int; order:int; parameters:float32[,][] option }

    type T = 
        | VAR of VAR 

    type Model = { ts:TS option ;
                   model:T;
                   graph:Graph;
                   }


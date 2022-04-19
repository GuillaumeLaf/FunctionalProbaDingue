namespace Timeseries

open FSharpPlus
open FSharpPlus.Data
open TimeseriesType
open TimeseriesState

module Transformations = 
    // Module to perform transformations on timeseries.
    // Should also update the 'Stats' object in the 'TS'.

    // Take the fractional difference with the given exponent
    let fractionalDifference alphas = (Array.map2 (Option.map2 (-))) <!> currentElements <*> lagElements 1      

    let totaldifference = (Array.map2 (Option.map2 (-))) <!> currentElements <*> lagElements 1      


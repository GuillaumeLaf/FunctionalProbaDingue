namespace Timeseries

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open TimeseriesType

// Everything that touches timeseries is implemented as multivariate operations.
// 'TS' are stored as 'Array2D' where the first index is the number of timeseries and
// the second is the length of each timeseries.
[<RequireQualifiedAccess>]
module TS = 
    
    let zeroCreate i j = (Array2D.zeroCreate i >> TS.create) j

    let get idx = TS.data >> Utils.Array2D.row idx 
    let atTime t ts = if (0 <= t) && (t < ts.Length) then Some ts.Data.[*,t] else None 
    let modifyAtTime t values ts = if (0 <= t || t < ts.Length) then { ts with Data= Utils.Array2D.setColumn t values ts.Data} else invalidArg "Index" "Time index greater than length of Timeseries."
    
    // Function to set a given statistic to the 'TS'.
    // 'statsFunc' is a function which output a 'Stats' from a given value and 'Stats'.
    // Modify the given 'Stats' by replacing a 'value' inside.
    let addStats statsFunc s ts = (TS.stats >> statsFunc s >> TS.setStats) ts ts

    let addCovariance = addStats Stats.setCov
    let addLowerCholeskyCov = addStats Stats.setLowerCholeskyCov



            





namespace Timeseries

open FSharpPlus
open FSharpPlus.Data

open TimeseriesType

// Everything that touches timeseries is implemented as multivariate operations.
// 'TS' are stored as 'Array2D' where the first index is the number of timeseries and
// the second is the length of each timeseries.
module TS = 

(*    let createUnivariate (ts:float32[]) = 
        let tmp = ts|> Array2D.ofSingleArray
        {length=ts.Length; size=1; data=tmp; stats=Statistics.Multivariate.Stats(tmp)}*)

(*    let univariateZeroCreate = Array.zeroCreate >> createUnivariate
    let multivariateZeroCreate i j = Array2D.zeroCreate i j |> createMultivariate*)
    
    let create i j = (Array2D.zeroCreate i >> TS.create) j

    let get idx = TS.data >> Array2D.row idx 
    let atTime t ts = if (0 <= t) && (t < ts.Length) then Some ts.Data.[*,t] else None 

    let modifyAtTime t values ts = if (0 <= t || t < ts.Length) then { ts with Data= Array2D.setCol t values ts.Data} else invalidArg "Index" "Time index greater than length of Timeseries."
    







            





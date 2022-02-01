namespace Timeseries

open FSharpPlus
open FSharpPlus.Data

open TimeseriesType

// Everything that touches timeseries is implemented as multivariate operations.
// 'TS' are stored as 'Array2D' where the first index is the number of timeseries and
// the second is the length of each timeseries.
module TS = 
    
    let length ts = ts.length
    let size ts = ts.size
    let data ts = ts.data
    let stats ts = ts.stats

    let createUnivariate (ts:float32[]) = 
        let tmp = ts|> Array2D.ofSingleArray
        {length=ts.Length; size=1; data=tmp; stats=Statistics.Multivariate.Stats(tmp)}

    let createMultivariate (ts:float32[,]) = {length=ts.[0,*].Length; size=ts.[*,0].Length; data=ts; stats=Statistics.Multivariate.Stats(ts)}

    let univariateZeroCreate = Array.zeroCreate >> createUnivariate
    let multivariateZeroCreate i j = Array2D.zeroCreate i j |> createMultivariate
    
    let get idx ts = ts.data.[idx,*]
    let atTime t ts = if (0 <= t || t < ts.length) then Some ts.data.[*,t] else None 

    let modifyAtTime t values ts = if (0 <= t || t < ts.length) then { ts with data= Array2D.setCol t values ts.data} else invalidArg "Index" "Time index greater than length of Timeseries."
    







            





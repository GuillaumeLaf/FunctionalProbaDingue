namespace Timeseries

open MathNet.Numerics.Statistics

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open TimeseriesType

[<RequireQualifiedAccess>]
module Stats = 
    // Module to create descriptive - and other - statistics about a timeseries. 

    module Computations = 
        // Module for computing statistics. 

        let Mean = Utils.Array2D.collectByRow Array.average
        let Std = Utils.Array2D.collectByRow (Statistics.StandardDeviation >> float32)        : (float32[,] -> float32[])
        let Cov (data:float32[,]) = Array2D.zeroCreate (Array2D.length1 data) (Array2D.length1 data) |> Array2D.mapi (fun i j _ -> Statistics.Covariance(data.[i,*],data.[j,*]) |> float32)
        
    module private StatsState =     
        // Module for managing a timeseries monad 

        let data = TS.data <!> State.get        : State<TS,float32[,]>
        let stats = TS.stats <!> State.get      : State<TS,Stats>

        // Memoization function for statistics computations
        // If the 'TS' monad state already has the computation then returns it, 
        // otherwise compute it and update the state with it. 
        let inline memoize statistic compute set = monad {
            let! s = stats
            let! d = data
            if (statistic s) = None then
                let result = compute d
                do! State.modify (fun t -> { t with Stats=set result s })
                return result
            else return (statistic >> Option.get) s
        }

        // Memoization function which takes a monad computation as argument and
        // base its computation based on the inner result of the given monad.
        // For instance, allows use of standard devation monad to compute the variance monad, 
        // and memoize the result of the variance monad in the 'TS' monad state. 
        // 'TS' state must have the possibility to memoize variance !. 
        let inline memoizeFromArg arg statistics compute set = arg >>= (fun a -> memoize statistics (flip compute a) set)
                                  
        let mean = memoize Stats.mean Computations.Mean Stats.setMean
        let std = memoize Stats.std Computations.Std Stats.setStd
        let var = std >>= (Array.map (fun x -> x*x)  >> result)
        let cov = memoize Stats.cov Computations.Cov Stats.setCov
        let lowerCholeskyCov = memoizeFromArg cov Stats.lowerCholeskyCov (fun _ -> Utils.cholesky) Stats.setLowerCholeskyCov
                                     
    let mean = State.run StatsState.mean 
    let std = State.run StatsState.std
    let var = State.run StatsState.var
    let cov = State.run StatsState.cov
    let lowerCholeskyCov = State.run StatsState.lowerCholeskyCov

    let onlyMean = mean >> fst
    let onlyStd = std >> fst
    let onlyVar = var >> fst
    let onlyCov = cov >> fst
    let onlyLowerCholeskyCov = lowerCholeskyCov >> fst
       

            



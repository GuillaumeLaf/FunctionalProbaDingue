namespace TimeSeries

open FSharpPlus
open FSharpPlus.Data

module TimeSeries = 
    
        // This module implements the manipulation associated with TimeSeries
        // The TimeSeries is access through a State Monad.
        // The State is composed of a tuple with the current index and the timeseries data as array(s).

        module Univariate = 
            // This submodule serves primarily as an interface for the multivariate methods.
            // Every univariate timeseries can be represented as a multivariate one.
            // However making the distinction will help when using the 'TimeSeries' module.

            let x = 0

        module Multivariate =
            // Everything that touches timeseries is implemented as multivariate operations.
            // The Monad State contains the current index (int) and the timeseries ('T[ts,time]:array2D option).
            // The first index of the 'State' corresponds to a particular time point. 
            // The second index of the 'State' correponds to a given timeseries.
            // By using 'array2D' to store the timeseries, 
            // it will be slightly more challenging to parallelize the code (cannot use Array.parallel).

            let currentIndex () = fst <!> State.get       : State<(int * 'T [,]),int> 
            let timeSeries () = snd <!> State.get       : State<(int * 'T [,]),'T [,]>
            let timeSeriesCount () = Array2D.length1 <!> timeSeries ()
            let periodCount () =  Array2D.length2 <!> timeSeries ()

            let currentElements () = Array2D.cols <!> currentIndex() <*> timeSeries()

            // This expression is expensive and will be used often.
            // I should find another way of getting lag elements (change data structure ?)
            let lagElements lag = monad {
                let! ts = timeSeries()
                let! currentidx = currentIndex()
                return match (currentidx-lag) with
                        | x when x < 0 -> Array.zeroCreate (Array2D.length1 ts)
                        | x when x >= Array2D.length2 ts -> Array.zeroCreate (Array2D.length1 ts)
                        | x -> ts.[*,x]
            }

            let leadElement lead = lagElements (-lead)

            let setIndex newIdx = State.modify (fun (idx,ts) -> (newIdx,ts))        : State<(int * 'T [,]),unit> 
            let incrementIndex () = setIndex 1
            





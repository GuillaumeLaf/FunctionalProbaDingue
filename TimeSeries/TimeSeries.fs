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
            // The Monad State contains the current index (int) and the timeseries ('T[,]:array2D option).
            // The first index corresponds to a particular time point. 
            // The second index correponds to a given timeseries.
            // By using 'array2D' to store the timeseries, 
            // it will be slightly more challenging to parallelize the code (cannot use Array.parallel).
            // Note the type 'Option' for the timeseries. 
            // In the multivariate case some obs may not be available for all timeseries.

            let currentIdx () = fst <!> State.get       : State<(int * 'T option [,]),int> 
            let timeSeries () = snd <!> State.get       : State<(int * 'T option [,]),'T option [,]>

            let currentElements () = Array2D.cols <!> currentIdx() <*> timeSeries()
            let lagElements lag = 
                (fun idx arrays -> )




namespace Timeseries

open FSharpPlus
open FSharpPlus.Data
open TimeseriesType

// Everything that touches timeseries is implemented as multivariate operations.
// The Monad State contains the current index (int) and the timeseries ('T[ts,time]:array2D option).
// The first index of the 'State' corresponds to a particular time point. 
// The second index of the 'State' correponds to a given timeseries.
// By using 'array2D' to store the timeseries, 
// it will be slightly more challenging to parallelize the code (cannot use Array.parallel).
module TimeseriesState = 
    
    // Get current Index
    let currentTime = fst <!> State.get       : State<(int * TS),int> 

    // Modify the current time
    let setTime newIdx = State.modify (fun (_,ts) -> (newIdx,ts))        : State<(int * TS),unit> 

    // Increment the current time by one.
    let incrementTime = State.modify (fun (idx,ts) -> (idx+1,ts))     : State<(int * TS),unit> 

    // Get the Array2D object representing the 'TimeSeries'
    let timeseries = snd <!> State.get       : State<(int * TS),TS>

    // Get the cross-sectional (or time) dimension
    let size = TS.size <!> timeseries
    let length = TS.length <!> timeseries

    // Extract the current cross-section
    let currentElements = TS.atTime <!> currentTime <*> timeseries

    // Set the current elements of the cross-section
    let setCurrentElements e = State.modify (fun (idx,ts) -> (idx,TS.modifyAtTime idx e ts))      : State<(int * TS),unit> 

    // Extract the cross-sections at a given lag (from the current time)
    let lagElements lag = TS.atTime <!> ((+) -lag <!> currentTime) <*> timeseries

    // Extract the cross-sections at a given lead in the future (from the current time)
    let leadElements lead = lagElements (-lead)

    // Extract a subrange of the 'idxTS'th timeseries with the current element along with 'lags' numbers of previous elements.
    let multipleLagElementsFor lags idxTS = Array.sub <!> (TS.get idxTS <!> timeseries) <*> (lift2 ( - ) currentTime (result lags)) <*> result (lags+1)


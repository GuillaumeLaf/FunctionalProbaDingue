namespace Timeseries

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control
open TimeseriesType

// Everything that touches timeseries is implemented as multivariate operations.
// The Monad State contains the current index (int) and the timeseries ('T[ts,time]:array2D option).
// The first index of the 'State' corresponds to a particular time point. 
// The second index of the 'State' correponds to a given timeseries.
// By using 'array2D' to store the timeseries, 
// it will be slightly more challenging to parallelize the code (cannot use Array.parallel).
module TimeseriesState = 
    
    // Get current Index
    let currentTime<'T> = fst <!> State.get       : State<(int * TS<'T>),int> 

    // Modify the current time
    let setTime<'T> newIdx = State.modify (fun (_,ts) -> (newIdx,ts))        : State<(int * TS<'T>),unit> 

    // Increment the current time by one.
    let incrementTime<'T> = State.modify (fun (idx,ts) -> (idx+1,ts))     : State<(int * TS<'T>),unit> 

    // Get the Array2D object representing the 'TimeSeries'
    let timeseries<'T> = snd <!> State.get       : State<(int * TS<'T>),TS<'T>>

    // Get the cross-sectional (or time) dimension
    let size<'T> = TS<'T>.size <!> timeseries<'T>
    let length<'T> = TS<'T>.length <!> timeseries<'T>

    let lastIndex<'T> = flip ( - ) 1 <!> length<'T>

    // Set the current data
    let setData<'T> (newData:'T[,]) = State.modify (fun ((idx:int),ts) -> (idx, TS<'T>.setData newData ts))

    // Extract the current cross-section
    let currentElements<'T> = TS<'T>.atTime <!> currentTime<'T> <*> timeseries<'T>

    // Set the current elements of the cross-section
    let setCurrentElements<'T> e = State.modify (fun (idx,ts) -> (idx,TS<'T>.modifyAtTime idx e ts))      : State<(int * TS<'T>),unit> 

    // Extract the cross-sections at a given lag (from the current time)
    let lagElements<'T> lag = TS<'T>.atTime <!> (flip (+) lag <!> currentTime<'T>) <*> timeseries<'T>

    // Extract the cross-sections at a given lead in the future (from the current time)
    let leadElements<'T> lead = lagElements<'T> (-lead)

    // Extract a subrange of the 'idxTS'th timeseries with the current element along with 'lags' numbers of previous elements.
    let multipleLagElementsFor<'T> lags idxTS = Array.sub <!> (TS<'T>.get idxTS <!> timeseries<'T>) <*> (flip ( - ) lags <!> currentTime<'T>) <*> result (lags+1)

    // Extract a subrange with the current element along with 'lags' numbers of previous elements.
    let multipleLagElements<'T> lags = 
        monad {
            let! (data:'T[,]) = TS<'T>.data <!> timeseries<'T>
            let! current = currentTime<'T>
            return data.[*,current-lags..current]
        }
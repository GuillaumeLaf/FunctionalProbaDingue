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
    let inline currentTime () = fst <!> State.get       : State<(int * TS< ^T >),int> 

    // Modify the current time
    let inline setTime newIdx = State.modify (fun (_,ts) -> (newIdx,ts))         : State<(int * TS< ^T >),unit> 

    // Increment the current time by one.
    let inline incrementTime () = State.modify (fun (idx,ts) -> (idx+1,ts))     : State<(int * TS< ^T >),unit> 

    // Get the Array2D object representing the 'TimeSeries'
    let inline timeseries () = snd <!> State.get       : State<(int * TS< ^T >),TS< ^T >>

    // Get the cross-sectional (or time) dimension
    let inline size () = TS< ^T >.size <!> timeseries()
    let inline length () = TS< ^T >.length <!> timeseries()

    let inline lastIndex () = flip ( - ) 1 <!> length()

    // Set the current data
    let inline setData (newData:^T[,]) = State.modify (fun ((idx:int),ts) -> (idx, TS< ^T >.setData newData ts))

    // Extract the current cross-section
    // let currentElements<float32 option> = curry TS<float32 option>.atTime <!> currentTime<'T> <*> timeseries<'T>
    let inline currentElements () = TS<_>.atTime <!> currentTime() <*> timeseries()

    // Set the current elements of the cross-section
    let inline setCurrentElements e = State.modify (fun (idx,ts) -> (idx,TS< ^T >.modifyAtTime idx e ts))      : State<(int * TS< ^T >),unit> 

    // Extract the cross-sections at a given lag (from the current time)
    let inline lagElements lag = TS< ^T >.atTime <!> (flip (+) lag <!> currentTime()) <*> timeseries()

    // Extract the cross-sections at a given lead in the future (from the current time)
    let inline leadElements lead = lagElements (-lead)

    // Extract a subrange of the 'idxTS'th timeseries with the current element along with 'lags' numbers of previous elements.
    let inline multipleLagElementsFor lags idxTS = Array.sub <!> (TS< ^T >.get idxTS <!> timeseries()) <*> (flip ( - ) lags <!> currentTime()) <*> result (lags+1)

    // Extract a subrange with the current element along with 'lags' numbers of previous elements.
    let inline multipleLagElements lags = 
        monad {
            let! (data:^T[,]) = TS< ^T >.data <!> timeseries()
            let! current = currentTime()
            return data.[*,current-lags..current]
        }
namespace TimeSeries

open FSharpPlus
open FSharpPlus.Data

    
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

        (*     Basic Multivariate Timeseries Object     *)
        type TS (tsIn:float32[,]) =
            let data = tsIn
            let stats = Statistics.Multivariate.Stats(tsIn)
            member this.Get () = data
            member this.Get idx = if (0 < idx || idx <= Array2D.length1 data) then Some data.[idx,*] else None 
            member this.Set idx newTs = if (0 < idx || idx <= Array2D.length1 data) then data.[idx,*] <- newTs else invalidArg "Idx" "cross-section index out of number of timeseries."
            member this.Get (a:int[]) = Array2D.init (Array.length a) (Array2D.length2 data) (fun i j -> data.[a.[i],j])
            member this.GetAtTime t = if (0 < t || t <= Array2D.length2 data) then Some data.[*,t] else None 
            member this.SetAtTime t values = if (0 < t || t <= Array2D.length2 data) then data.[*,t] <- values else invalidArg "t" "Time index out of current timeseries length." 
            member this.Stats = stats

            static member empty n = Array2D.zeroCreate n 0 |> TS
            static member init n t = Array2D.zeroCreate n t |> TS
            static member length (ts:TS) = ts.Get () |> Array2D.length2
            static member size (ts:TS) = ts.Get () |> Array2D.length1
            static member get (ts:TS) = ts.Get ()      
            static member getAtTime t (ts:TS) = ts.GetAtTime t
            static member setAtTime t values (ts:TS) = ts.SetAtTime t values

        (*****************************************************************************************)
        //                                                                                       //
        //      This part corresponds to the Monad associated with a multivariate 'TimeSeries'   //
        //                                                                                       //
        (*****************************************************************************************)

        (*   Basic Operations   *)

        // Get current Index
        let currentTime = fst <!> State.get       : State<(int * TS),int> 

        // Modify the current time
        let setTime newIdx = State.modify (fun (_,ts) -> (newIdx,ts))        : State<(int * TS),unit> 

        // Increment the current time by one.
        let incrementTime = State.modify (fun (idx,ts) -> (idx+1,ts))     : State<(int * TS),unit> 

        // Get the Array2D object representing the 'TimeSeries'
        let timeSeries = snd <!> State.get       : State<(int * TS),TS>

        // Get the cross-sectional (or time) dimension
        let crossSectionDim = (TS.get >> Array2D.length1) <!> timeSeries
        let timeDim = (TS.get >> Array2D.length2) <!> timeSeries

        // Extract the current cross-section
        let currentElements = TS.getAtTime <!> currentTime <*> timeSeries

        // Set the current elements of the cross-section
        let setCurrentElements e = State.modify (fun (idx,ts) -> ts.SetAtTime idx e; (idx,ts))      : State<(int * TS),unit> 

        // Extract the cross-sections at a given lag (from the current time)
        let lagElements lag = TS.getAtTime <!> ((+) -lag <!> currentTime) <*> timeSeries
        let lagElementsDefault lag = Option.defaultValue <!> (Array.zeroCreate <!> crossSectionDim) <*> lagElements lag

        // Extract the cross-sections at a given lead in the future (from the current time)
        let leadElements lead = lagElements (-lead)

            





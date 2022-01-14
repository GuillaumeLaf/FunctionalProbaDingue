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
        type TS<'T> (tsIn:'T[,]) =
            let data = tsIn
            member this.Get () = data
            member this.Get idx = if (0 < idx || idx <= Array2D.length1 data) then Some data.[idx,*] else None 
            member this.Get (a:int[]) = Array2D.init (Array.length a) (Array2D.length2 data) (fun i j -> data.[a.[i],j])
            member this.GetTime t = if (0 < t || t <= Array2D.length2 data) then Some data.[*,t] else None 
                
                 
            static member length (ts:TS<'T>) = ts.Get () |> Array2D.length1
            static member size (ts:TS<'T>) = ts.Get () |> Array2D.length2
            static member get (ts:TS<'T>) = ts.Get ()      
            static member getTime t (ts:TS<'T>) = ts.GetTime t

        (*****************************************************************************************)
        //                                                                                       //
        //      This part corresponds to the Monad associated with a multivariate 'TimeSeries'   //
        //                                                                                       //
        (*****************************************************************************************)

        (*   Basic Operations   *)

        // Get current Index
        let currentTime () = fst <!> State.get       : State<(int * TS<'T>),int> 

        // Get the Array2D object representing the 'TimeSeries'
        let timeSeries () = snd <!> State.get       : State<(int * TS<'T>),TS<'T>>

        // Get the cross-sectional (or time) dimension
        let crossSectionDim () = (TS.get >> Array2D.length1) <!> (timeSeries ())
        let timeDim () =  (TS.get >> Array2D.length2) <!> (timeSeries ())

        // Extract the current cross-section
        let currentElements () = TS.getTime <!> currentTime() <*> timeSeries()

        // Extract the cross-sections at a given lag (from the current time)
        let lagElements lag = TS.getTime <!> ((+) -lag <!> currentTime()) <*> timeSeries()

        // Extract the cross-sections at a given lead in the future (from the current time)
        let leadElements lead = lagElements (-lead)

        // Modify the current time
        let setIndex newIdx = State.modify (fun (_,ts) -> (newIdx,ts))        : State<(int * TS<'T>),unit> 

        // Increment the current time by one.
        let incrementIndex () = State.modify (fun (idx,ts) -> (idx+1,ts))     : State<(int * TS<'T>),unit> 
            





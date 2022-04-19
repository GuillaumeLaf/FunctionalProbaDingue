namespace Timeseries

open FSharpPlus
open FSharpPlus.Data
open TimeseriesType
open TimeseriesState

module Transformations = 
    // Module to perform transformations on timeseries.
    // Should also update the 'Stats' object in the 'TS'.

    module Computations =
        // Get fractional coefficients (in reverse order : coeff for current time is last in array).
        let fractionalDiffCoeffs (d:float32) (thresh:float32) = 
            let rec loop lag l = 
                match l with
                | x::_ when abs(x) <= thresh -> l |> Array.ofList
                | x::xs -> loop (lag+1f) (((lag-1f-d)/lag)*x::l)
                | [] -> failwith "Impossible to have empty list in fractional coeffs." 
            loop 2f [d]

    // Take the fractional difference with the given exponents
    // Check the formula, some coefficients should be negative but aren't
    // Compute the coefficients for each call of this function (which only treats the current time !) 
    // the function should rather take the already computed coefficients.
    let fractionalDifference (ds:float32[]) (thresh:float32) = 
        monad {
            let out = Array.zeroCreate (Array.length ds)
            for i in 0..Array.length ds-1 do
                let c = Computations.fractionalDiffCoeffs ds.[i] thresh
                let! (e:float32 option[]) = multipleLagElementsFor (Array.length c) i                          
                out.[i] <- Array.foldBack2 (fun coeff elem s -> ( * ) coeff <!> elem |> lift2 ( + ) s) c e (Some 0f)
            return out
        }

    let totalDifference = ((-) |> Option.map2 |> Array.map2) <!> currentElements <*> lagElements 1  
    
    let center mean = ((-) |> Option.map2 |> Array.map2) <!> currentElements <*> result mean

    let standardize std = ((/) |> Option.map2 |> Array.map2) <!> currentElements <*> result std





namespace Timeseries

open FSharpPlus
open FSharpPlus.Data
open TimeseriesType
open TimeseriesState

module Transformations = 
    // Module to perform transformations on timeseries.
    // Should also update the 'Stats' object in the 'TS'.
    [<RequireQualifiedAccess>]
    module Computations =
        // Get fractional coefficients (in reverse order : coeff for current time is last in array).
        // To allow inverse transfo. 'd' should be in (0, 0.5). 
        // https://link.springer.com/article/10.1007/s12572-021-00299-5
        let fractionalDiffCoeffs (thresh:float32) (d:float32) = 
            let rec loop lag l = 
                match l with
                | x::xs when abs(x) <= thresh -> xs |> Array.ofList
                | x::_ -> loop (lag+1f) ((-x)*((d-lag+1f)/lag)::l)
                | [] -> failwith "Impossible to have empty list in fractional coeffs." 
            loop 2f [-d]

    [<RequireQualifiedAccess>]
    module SingleObservation = 
        // Make sure that we don't recompute constants for each observations (e.g. means, std, ... of the whole ts).

        // Take the fractional difference with the given exponents
        let fractionalDifference (coeffs:float32[][]) = 
            monad {
                let out = Array.zeroCreate (Array.length coeffs)
                for i in 0..Array.length coeffs-1 do
                    let! (e:float32 option[]) = multipleLagElementsFor (Array.length coeffs.[i]) i                          
                    out.[i] <- Array.foldBack2 (fun coeff elem s -> ( * ) coeff <!> elem |> lift2 ( + ) s) coeffs.[i] e (Some 0f)
                return out
            }

        let totalDifference = ((-) |> Option.map2 |> Array.map2) <!> currentElements <*> lagElements 1  
    
        let center means = ((-) |> Option.map2 |> Array.map2) <!> currentElements <*> result means

        let standardize stds = ((/) |> Option.map2 |> Array.map2) <!> currentElements <*> result stds


    // Possible Parallelization, since isn't carried along.
    let inline traverse2 compute arg1M arg2M save = 
        monad {
            let! arg1 = arg1M
            let! arg2 = arg2M
            let! out = Array2D.zeroCreate <!> size <*> length
            for i in 0..Array2D.length2 out-1 do
                do! setTime i
                let! c = compute arg1 arg2
                out.[*,i] <- c
            return! (TS.setData out >> TS.addTransformation (save arg1 arg2)) <!> timeseries
        }

    let traverse1 compute argM save = traverse2 (fun a _ -> compute a) argM (result ()) (fun a _ -> save a)
    let traverse compute save = traverse1 (fun _ -> compute) (result ()) (fun _ -> save)

    let center = traverse1 SingleObservation.center 
                           Stats.Monad.mean 
                           (Some >> Center)

    let standardize = traverse1 SingleObservation.standardize 
                                Stats.Monad.std 
                                (Some >> Standardize)

    let totalDifference = traverse SingleObservation.totalDifference 
                                   TotalDifference

    let fractionalDifference ds thresh = traverse1 SingleObservation.fractionalDifference 
                                                   ((Computations.fractionalDiffCoeffs >> Array.map) thresh ds |> result) 
                                                   (fun _ -> FracDifference(Some ds,thresh))

    let forward transforms ts = 
        let rec loop remaining ts = 
            match remaining with
            | [] -> ts
            | Center(_)::xs -> (State.eval center (0,ts)) |> loop xs
            | Standardize(_)::xs -> (State.eval standardize (0,ts)) |> loop xs
            | TotalDifference::xs -> (State.eval totalDifference (0,ts)) |> loop xs
            | FracDifference(ds,thresh)::xs -> (State.eval (fractionalDifference ds.Value thresh) (0,ts)) |> loop xs // 'ds' is unboxed unsafely
        loop transforms ts

    let backward ts = 0



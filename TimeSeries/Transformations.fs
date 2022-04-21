namespace Timeseries

open FSharpPlus
open FSharpPlus.Data
open TimeseriesType
open TimeseriesState

module Transformations = 
    // Module to perform transformations on timeseries.
    // Should also update the 'Stats' object in the 'TS'.
    [<RequireQualifiedAccess>]
    module Utils =
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
    module Forward = 
        // Monads to transform
        // Make sure that we don't recompute constants for each observations (e.g. means, std, ... of the whole ts).

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

        module Single = 
            // Compute the transform for a single observation (given the whole TS).
            let totalDifference = (( - ) |> (Option.map2 >> Array.map2)) <!> currentElements <*> lagElements 1  
    
            let center means = (( - ) |> (Option.map2 >> Array.map2)) <!> currentElements <*> result means

            let standardize stds = (( / ) |> (Option.map2 >> Array.map2)) <!> currentElements <*> result stds

            // Take the fractional difference with the given exponents
            let fractionalDifference (coeffs:float32[][]) = 
                monad {
                    let out = Array.zeroCreate (Array.length coeffs)
                    for i in 0..Array.length coeffs-1 do
                        let! (e:float32 option[]) = multipleLagElementsFor (Array.length coeffs.[i]) i                          
                        out.[i] <- Array.foldBack2 (fun coeff elem s -> ( * ) coeff <!> elem |> lift2 ( + ) s) coeffs.[i] e (Some 0f)
                    return out
                }

        let center = traverse1 Single.center 
                               Stats.Monad.mean 
                               (Some >> Center)

        let standardize = traverse1 Single.standardize 
                                          Stats.Monad.std 
                                          (Some >> Standardize)

        let totalDifference = traverse1 (fun _ -> Single.totalDifference)
                                              (TS.atTime 0 <!> timeseries)
                                              (Some >> TotalDifference)

        let fractionalDifference ds thresh = traverse1 Single.fractionalDifference 
                                                             ((Utils.fractionalDiffCoeffs >> Array.map) thresh ds |> result) 
                                                             (fun _ -> FracDifference(Some ds,thresh))

    [<RequireQualifiedAccess>]
    module Backward = 
        // Monads allowing reverse transform
        
        // Possible Parallelization, since isn't carried along.
        let inline traverse2 compute arg1 arg2 = 
            monad {
                let! out = Array2D.zeroCreate <!> size <*> length
                for i in 0..Array2D.length2 out-1 do
                    do! setTime i
                    let! c = compute arg1 arg2
                    out.[*,i] <- c
                return! TS.setData out <!> timeseries
            }

        let traverse1 compute arg = traverse2 (fun a _ -> compute a) arg ()
        let traverse compute = traverse1 (fun _ -> compute) ()

        module Single =
            // Reverse the transformation for a single observation (given the whole TS). 
            let totalDifference = (( + ) |> (Option.map2 >> Array.map2)) <!> currentElements <*> lagElements 1  

            let center means = (( + ) |> (Option.map2 >> Array.map2)) <!> currentElements <*> result means

            let standardize stds = (( * ) |> (Option.map2 >> Array.map2)) <!> currentElements <*> result stds

            let fractionalDifference = Forward.Single.fractionalDifference

        let center = traverse1 Single.center 

        let standardize = traverse1 Single.standardize

        let totalDifference firsts = 
            monad {
                let! t = currentTime
                if t = 0 then 
                    return firsts
                else 
                    return! Single.totalDifference
            } |> traverse

        // To allow inverse transfo. 'd' should be in (0, 0.5). 
        // Get inverse from negating 'd'.
        let fractionalDifference ds thresh = traverse1 Single.fractionalDifference 
                                                       ((( * ) -1f >> Utils.fractionalDiffCoeffs >> Array.map) thresh ds) 
                                   
    
    // Transfrom a timeseries, then save the transformation made in 'TS',
    // such that, the transformation reversible
    let forward transforms ts = 
        let rec loop remaining ts = 
            match remaining with
            | [] -> ts
            | Center(_)::xs -> (State.eval Forward.center (0,ts)) |> loop xs
            | Standardize(_)::xs -> (State.eval Forward.standardize (0,ts)) |> loop xs
            | TotalDifference(_)::xs -> (State.eval Forward.totalDifference (0,ts)) |> loop xs
            | FracDifference(ds,thresh)::xs -> (State.eval (Forward.fractionalDifference ds.Value thresh) (0,ts)) |> loop xs // 'ds' is unboxed unsafely
        loop transforms ts

    // Reverse the transformation through the list saved in 'TS'
    let backward ts = 
        let rec loop remaining ts = 
            match remaining with
            | [] -> ts
            | Center(means)::xs -> (State.eval (Backward.center means.Value) (0,ts)) |> loop xs         // unboxed unsafely
            | Standardize(stds)::xs -> (State.eval (Backward.standardize stds.Value) (0,ts)) |> loop xs     // unboxed unsafely
            | TotalDifference(firsts)::xs -> (State.eval (Backward.totalDifference firsts.Value) (0,ts)) |> loop xs     // unboxed unsafely
            | FracDifference(ds,thresh)::xs -> (State.eval (Backward.fractionalDifference ds.Value thresh) (0,ts)) |> loop xs // 'ds' is unboxed unsafely
        loop ts


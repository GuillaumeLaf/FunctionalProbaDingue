namespace Timeseries

open FSharpPlus
open FSharpPlus.Data
open TimeseriesType
open TimeseriesState

[<RequireQualifiedAccess>]
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

            // Append idx to array list if changed with default
            // Use Inplace Array modification
            let defaultWith value (array:int list[]) = 
                let tmp currentIdx i = function
                    | Some _ as x -> x
                    | None -> array.[i] <- currentIdx::array.[i]
                              Some value
                monad {
                    let! idx = currentTime
                    return! Array.mapi (tmp idx) <!> currentElements
                }

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
                                >>= (fun ts -> TS.setData (ts.Data.[*,1..]) ts |> result)

        let fractionalDifference ds thresh = traverse1 Single.fractionalDifference 
                                                       ((Utils.fractionalDiffCoeffs >> Array.map) thresh ds |> result) 
                                                       (fun _ -> FracDifference(Some ds,thresh))

        let defaultWith value = traverse1 (Single.defaultWith value)
                                          (Array.create <!> size <*> result [])
                                          (fun indices -> DefaultWith(value, Some indices))
                                          

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

        // The main difference with 'traverse' is that the provided monad directly modifies the data of the state.
        // 'compute' monad computation can depend on previously computed values (and not only on the original provided state).
        let inline accumulate compute = 
            monad {
                let! len = length
                for i in 0..len-1 do
                    do! setTime i
                    let! c = compute
                    do! setCurrentElements c
                return! timeseries
            }

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
                let! len = length
                match t with
                | 0 -> let! (out:float32 option [,]) = Array2D.zeroCreate <!> size <*> (( + ) 1 <!> length)         
                       let! (data:float32 option [,]) = TS.data <!> timeseries                                
                       out.[*,0] <- firsts
                       out.[*,1..] <- data
                       do! setData out
                       return firsts

                // Special treatment of the last element of array (since array was lengthened).
                | x when x=len-2 -> let! c = Single.totalDifference   
                                    do! setCurrentElements c 
                                    do! setTime (len-1) 
                                    do! Single.totalDifference >>= setCurrentElements 
                                    do! setTime (len-2) 
                                    return c
                | _ -> return! Single.totalDifference  
            } |> accumulate

        // To allow inverse transfo. 'd' should be in (0, 0.5). 
        // Get inverse from negating 'd'.
        let fractionalDifference ds thresh = traverse1 Single.fractionalDifference 
                                                       ((( * ) -1f >> Utils.fractionalDiffCoeffs >> Array.map) thresh ds) 
                                   
    
    // Transfrom a timeseries, then save the transformation made in 'TS',
    // such that, the transformation reversible
    let forward transforms ts = 
        let inline apply loop ts xs m = State.eval m (0,ts) |> loop xs
        let rec loop remaining ts = 
            match remaining with
            | [] -> ts
            | Center(_)::xs -> apply loop ts xs Forward.center
            | Standardize(_)::xs -> apply loop ts xs Forward.standardize
            | TotalDifference(_)::xs -> apply loop ts xs Forward.totalDifference
            | FracDifference(ds,thresh)::xs -> apply loop ts xs (Forward.fractionalDifference ds.Value thresh) // 'ds' is unboxed unsafely
            | DefaultWith(value,_)::xs -> apply loop ts xs (Forward.defaultWith value)
        loop transforms ts

    // Reverse the transformation through the list saved in 'TS'
    let backward ts = 
        let apply loop ts xs m = State.eval m (0,ts) |> loop xs
        let rec loop remaining ts = 
            match remaining with
            | [] -> ts
            | Center(means)::xs -> apply loop ts xs (Backward.center means.Value)        // unboxed unsafely
            | Standardize(stds)::xs -> apply loop ts xs (Backward.standardize stds.Value)     // unboxed unsafely
            | TotalDifference(firsts)::xs -> apply loop ts xs (Backward.totalDifference firsts.Value)    // unboxed unsafely
            | FracDifference(ds,thresh)::xs -> apply loop ts xs (Backward.fractionalDifference ds.Value thresh) // 'ds' is unboxed unsafely
        loop ts.Transformation ts


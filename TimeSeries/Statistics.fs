namespace Timeseries

open MathNet.Numerics.Statistics

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open TimeseriesType

[<RequireQualifiedAccess>]
module Stats = 
    // Module to create descriptive - and other - statistics about a timeseries. 

    module Computations = 
        let crossSum f array1 array2 = Array.foldBack2 (Option.foldBack2 (fun x1 x2 s -> f x1 x2 + s)) array1 array2 0f
        let sum f array = crossSum (fun _ -> f) array array
        let countSome array = Array.foldBack (Option.count >> (+)) array 0 |> float32
    
        let mean array = flip (/) (countSome array) (sum id array)
        let cov array1 array2 = (fun mean1 mean2 -> crossSum (fun x1 x2 -> (x1-mean1)*(x2-mean2)) array1 array2) (mean array1) (mean array2) |> ( * ) (1.0f/(countSome array1-1.0f))
        let var array = (fun mean -> sum (fun x -> (x-mean)*(x-mean)) array) (mean array) |> ( * ) (1.0f/(countSome array-1.0f))
        let std = var >> sqrt

    let mean (ts:TS) = Array2D.collectByRow Computations.mean ts.Data
    let var (ts:TS) = Array2D.collectByRow Computations.var ts.Data
    let std (ts:TS) = Array2D.collectByRow Computations.std ts.Data
    let cov (ts:TS) = Array2D.zeroCreate (ts.Size) (ts.Size) |> Array2D.mapi (fun i j _ -> Computations.cov ts.Data.[i,*] ts.Data.[j,*])
    let cholesky = cov >> Utils.cholesky 

   

            



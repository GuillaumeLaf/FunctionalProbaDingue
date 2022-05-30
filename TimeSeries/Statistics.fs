namespace Timeseries

open MathNet.Numerics.Statistics

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open TimeseriesType
open TimeseriesState

[<RequireQualifiedAccess>]
module Stats = 
    // Module to create descriptive - and other - statistics about a timeseries. 

    module Computations = 
        let inline crossSum (f:^T -> ^T -> ^T) (array1:^T[]) (array2:^T[]) = Array.foldBack2 (fun x1 x2 s -> f x1 x2 + s) array1 array2 Unchecked.defaultof< ^T >
        let inline sum f array = crossSum (fun _ -> f) array array
    
        let inline mean (array:^T[]) = Array.average array
        let inline cov array1 array2 = (fun mean1 mean2 -> crossSum (fun x1 x2 -> (x1-mean1)*(x2-mean2)) array1 array2) (mean array1) (mean array2) |> flip LanguagePrimitives.DivideByInt (array1.Length-1)
        let inline var array = (fun mean -> sum (fun x -> (x-mean)*(x-mean)) array) (mean array) |> flip LanguagePrimitives.DivideByInt (array.Length-1)
        
    let inline mean (ts:TS< ^T >) = Array2D.collectByRow Computations.mean ts.Data
    let inline var (ts:TS< ^T >) = Array2D.collectByRow Computations.var ts.Data
    let inline cov (ts:TS< ^T >) = Array2D.zeroCreate (ts.Size) (ts.Size) |> Array2D.mapi (fun i j _ -> Computations.cov ts.Data.[i,*] ts.Data.[j,*])
    let inline cholesky array = cov >> Utils.cholesky <| array


    module Monad = 
        // Monad form of statistics 

        let inline mean () = mean <!> timeseries()
        let inline var () = var <!> timeseries()
        let inline std () = Array.map sqrt <!> var()
        let inline cov () = cov <!> timeseries()
        





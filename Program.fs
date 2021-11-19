// Learn more about F# at http://fsharp.org

open System
open TimeSeries
open Models
open Monads
open FSharp.Charting
open Binance

open DataBase

open MathNet.Numerics
open MathNet.Numerics.IntegralTransforms

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

(*    let initTS = Array.init 30 (fun idx -> (float >> Some) idx)
    let initInnov = Array.init 30 (fun idx -> Some 0.0)
    let initState = Univariate.State(0, initTS, initInnov, [])
    let x, s = Monad.run (Transformations.normalizeM) initState
    printfn "%A" s
    printfn "%A" (Monad.run (Transformations.inverseTransformationsM ()) s)*)
    let arr = Array.init 30 (fun idx -> float idx)
    Fourier.Forward(arr, Array.zeroCreate 30, FourierOptions.Default)
    printfn "%A" (arr)



    
    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code
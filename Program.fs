// Learn more about F# at http://fsharp.org

open System
open TimeSeries
open Models
open Monads
open FSharp.Charting

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let modelSK = MonadicGraph.defaultSkeletonForSampling (Array.zeroCreate 300 |> ARp)
    let defaultGraphState = MonadicGraph.State(Array.zeroCreate 300, Array.init 300 (fun x->float x),[|1.0|])
    printfn "%A" (Monad.run (MonadicGraph.skeletonGradientM modelSK) defaultGraphState)

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code
// Learn more about F# at http://fsharp.org

open System
open TimeSeries
open Models
open Monads

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let modelName = AR(1)
    let modelSkeleton = MonadicGraph.defaultSkeleton modelName
    let modelM = MonadicGraph.modelM modelName MonadicGraph.Sampling

    let graphTSM = GraphTimeSeries.runModelM modelM

    let defaultStates = GraphTimeSeries.defaultState modelName 500
    printfn "%A" (defaultStates ||> BiMonad.run graphTSM)

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code
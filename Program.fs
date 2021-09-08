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

    let updateVarM = GraphTimeSeries.updateVariablesM modelName 

    let graphBM = GraphTimeSeries.sampleOnceM modelM updateVarM

    let (stateTS,stateG) = GraphTimeSeries.defaultState modelName 1000
    //let stateG = MonadicGraph.State([|0.7|],[|0.0|])

    printfn "%A" (GraphTimeSeries.foldRun graphBM stateTS stateG)

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code
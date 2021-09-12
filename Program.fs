// Learn more about F# at http://fsharp.org

open System
open TimeSeries
open Models
open Monads

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let samplingModel = ARp([|0.7|]) |> Sampling
    let (TimeSeries.UnivariateTimeSeries.State(_,data,innov)) = GraphTimeSeries.sample 1000 samplingModel

    let fittingModel = AR(1)
    let fittedModel = Optimization.fit fittingModel data
    printfn "%A" (fittedModel)

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code
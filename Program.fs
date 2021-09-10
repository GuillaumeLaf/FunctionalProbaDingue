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

    let fittingModel = AR(1) |> Fitting
    let (TimeSeries.UnivariateTimeSeries.State(_,data2,error)) = GraphTimeSeries.getError data fittingModel
    let finalError = error |> Array.map (fun x -> Option.defaultValue 0.0 x)

    printfn "%A" (Utilities.autocorrelation 10 finalError)
    printfn "%A" (TimeSeries.UnivariateTimeSeries.State(0,data2,error))

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code
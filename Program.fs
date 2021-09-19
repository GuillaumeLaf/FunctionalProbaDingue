// Learn more about F# at http://fsharp.org

open System
open TimeSeries
open Models
open Monads
open FSharp.Charting

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    //let samplingModel = STARp([|0.5|],[|-0.7|],0.0,1.0,ARp([|0.7|])) |> Sampling
    let samplingModel = ARp([|0.7|]) |> Sampling
    //let (TimeSeries.UnivariateTimeSeries.State(_,data,innov)) = GraphTimeSeries.sample 2500 samplingModel
    //let plotData = data |> Array.map (fun x -> Option.defaultValue 0.0 x)
    let sampleTS = GraphTimeSeries.sample 10000 samplingModel
    //let result,nxtState = (Monad.run (TimeSeries.UnivariateTimeSeries.normalizeM) sampleTS)
    // Chart.Line plotData |> Chart.Show
    let (TimeSeries.UnivariateTimeSeries.State(_,data,innov)) = sampleTS

    //let fittingModel = STAR(1,0.0,1.0,AR(1))
    let fittingModel = AR(1)
    let fittedModel = Optimization.fit fittingModel data
    printfn "%A" (fittedModel)

(*    let sk = MonadicGraph.defaultSkeletonForSampling (STARp([|0.7|],[|0.7|],0.0,1.0,ARp([|0.7|])))
    //let sk = MonadicGraph.defaultSkeletonForSampling (ARp(Array.zeroCreate 5|))
    DrawStructure.Model sk*)

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code
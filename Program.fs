// Learn more about F# at http://fsharp.org

open System
open Models
open Distributions
open Utilities
open PSO
open Models
open Backtester
open FSharp.Charting


[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let ghostModel = Model.build SETAR (SETARparams([|0.6|], [|-0.5;0.0|], 0.0, 2))
    // let ghostModel = Model.build AR (ARparams([|0.5|]))
    let sple = Model.sample 1000 ghostModel |> Array.scan (+) 0.0
    let spleChart = Chart.Line sple
    // spleChart |> Chart.Show
    
    let settings = {strategy=ModelStrategy(ghostModel);positionSizeStrategy=NaivePosition;windowSize=750}
    let backtestResults = Backtester.run settings sple
    let pnl = Backtester.computePnL backtestResults |> Array.scan (+) 0.0
    let pos = Backtester.computePositionHistory backtestResults

    let PnLChart = Chart.Line pnl
    let positionChart = Chart.Line pos
    
    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    PnLChart |> Chart.Show

    0 // return an integer exit code

// Learn more about F# at http://fsharp.org
open XPlot.GoogleCharts

open System
open Models
open Distributions
open Utilities
open PSO
open Backtester
open XPlot.Plotly

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let ghostModel = Model.build SETAR (SETARparams([|0.6|], [|-1.5|], 0.5, 1))
    // printfn "%A" ghostModel
    let sple = ghostModel |> Model.sample 1000
                          // |> Array.scan (+) 0.0
    Chart.Line sple |> Chart.Show

    let fitModel = Model.build SETAR (SETARparams([|0.0|], [|0.0|], 0.0, 1))
    let fittedModel = fitModel |> Model.fit sple

    let condExpect = Model.conditionalExpectation 20 fittedModel
    let arr = Array.concat [|sple.[sple.Length-20..sple.Length-1] ;condExpect|]

    Chart.Line arr |> Chart.Show

(*    let settings = {strategy=ModelStrategy(ghostModel);positionSizeStrategy=NaivePosition;windowSize=500}

    let samples = Array.zeroCreate 10 |> Array.map (fun x -> ghostModel|> Models.sample 1500 |> Array.scan (+) 0.0)
    let Runs = samples |> Array.Parallel.map (fun x -> Backtester.run settings x)
    let PnLs = Runs |> Array.map (fun x -> x |> Backtester.computePnL |> Array.fold (+) 0.0)
    printfn "%A" PnLs *)


    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)
    0 // return an integer exit code

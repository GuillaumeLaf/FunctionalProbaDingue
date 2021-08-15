// Learn more about F# at http://fsharp.org
open XPlot.GoogleCharts

open System
open Models
open Distributions
open Utilities
open PSO
open Models
open Backtester
open XPlot.Plotly


[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    // let ghostModel = Model.build SETAR (SETARparams([|0.6;0.0;0.0|], [|-1.5;0.0;0.0|], 0.5, 1))
    let ghostModel = Model.build AR (ARparams([|0.6;0.0;0.0;0.0;0.0|]))
    DrawModel.DrawModel ghostModel
    // printfn "%A" ghostModel
    // let sple = ghostModel |> Model.sample 1000
                          // |> Array.scan (+) 0.0
(*    Chart.Line sple |> Chart.Show

    let fitModel = Model.build SETAR (SETARparams([|0.0|], [|0.0|], 0.0, 1))
    let fittedModel = fitModel |> Model.fit sple*)



    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)


    0 // return an integer exit code

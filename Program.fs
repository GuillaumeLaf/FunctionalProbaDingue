// Learn more about F# at http://fsharp.org

open System
open TimeSeries
open Models
open Monads
open FSharp.Charting

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let model = ARp([|0.7|]) |> Sampling
    let (Univariate.State(idx,data,innov)) = GraphTimeSeries.sample 100000 model
    printfn "%A" (Models.SGD.fit (AR(1)) 0.01 40 data)
    

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code
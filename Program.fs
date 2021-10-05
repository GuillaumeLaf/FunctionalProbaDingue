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
    let initStateG = Graph.defaultState model
    let initStateTS = TimeSeries.Univariate.defaultState 10
    let skM = Graph.modelM model
    let updateM = GraphTS.defineUpdatesM (ARp([|0.0|]))

    printfn "%A" (Monad.run (GraphTS.sampleOnceM updateM skM) (initStateG,initStateTS))

    

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code
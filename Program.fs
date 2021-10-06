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
    let spleStates = GraphTS.sample 10000 model

(*    let fittingModel = AR(1)
    let (_,TimeSeries.Univariate.State(_,_,error)) = GraphTS.getError fittingModel sple (Graph.State([|0.0|],[|0.0|],[|0.0|]))
    let error = Array.map (fun x -> x |> Option.defaultValue 0.0) error
    let error = Array.map (fun x -> x*x) error
    printfn "%A" (UtilitiesSIMD.ArraySIMD.sum error)*)


    

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code
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
    let sple = GraphTS.sample 10000 model
    let sple = Array.map (fun x -> Some x) sple

    printfn "%A" (SGD.fit (AR(2)) 0.001 50 sple)


    

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code
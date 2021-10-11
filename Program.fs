// Learn more about F# at http://fsharp.org

open System
open TimeSeries
open Models
open Monads
open FSharp.Charting

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let model = STARp([|0.6;-0.2|],[|-0.9;0.4|],0.0,1.0,ARp([|0.7|])) |> Sampling
    //let model = ARp([|0.7|]) |> Sampling
    let sple = GraphTS.sample 1000 model
    let sple = Array.map (fun x -> Some x) sple

    //printfn "%A" (SGD.fit (AR(1)) 0.0001 100 sple)
    printfn "%A" (SGD.fit (STAR(2,0.0,1.0,AR(1))) 0.005 100 sple)


    

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code
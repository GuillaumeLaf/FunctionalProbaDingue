// Learn more about F# at http://fsharp.org

open System
open Models
open CSVManagement
open Distributions
open Utilities
open PSO
open Models
open Backtester
open FSharp.Charting

open FSharp.Data


[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    // let ghostModel = Model.build SETAR (SETARparams([|0.6|], [|-1.5|], 0.0, 1))
    let ghostModel = Model.build AR (ARparams([|-0.7|]))
    let (T(_,(Graph(state,sk)),_)) = ghostModel
    let sple = (Model.sample 1000 ghostModel)

    let m = Model.build AR (ARparams([|0.0|]))
    printfn "%A" (Optimization.fit sple m)

    
    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code

// Learn more about F# at http://fsharp.org

open System
open TimeSeries
open Models
open Monads
open FSharp.Charting

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let ts = TimeSeries.Univariate.State(0,Array.init 10 (fun i -> Some (float i)),Array.zeroCreate 10)
    printfn "%A" ts

    printfn "%A" (Monad.run Transformations.normalizeM ts)

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code
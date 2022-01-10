
open System
open ComputationalGraph
open TimeSeries
open FSharpPlus.Data
open FSharpPlus

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let stateArray = Array.zeroCreate 100000000
    let s = TimeSeries.S(stateArray)
    ignore (State.run (TimeSeries.fold 10000) s |> fst)

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code


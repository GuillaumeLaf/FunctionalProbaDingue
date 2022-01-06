
open System
open ComputationalGraph
open FSharpPlus.Data
open FSharpPlus

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    printfn "%A" (foldMap (compare 2) [1;2;3])

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code


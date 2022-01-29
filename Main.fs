
open System
open FSharpPlus.Data
open FSharpPlus
open ComputationalGraph.Graph

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let g = Input(Parameter(0,0)) + Input(Parameter(0,1)) + Input(Parameter(0,2)) * Input(Parameter(0,3))
    printfn "%A" (Graph.DefaultState g)

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code


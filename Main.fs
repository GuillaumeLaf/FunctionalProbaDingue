open System
open FSharpPlus.Data
open FSharpPlus
open ComputationalGraph
open ComputationalGraph.GraphType
open Models
open Models.ModelType
open Utils

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let g = Input(Parameter(0,0)) + Input(Parameter(0,1)) * (Input(Parameter(0,1))*Input(Parameter(0,2)) + Input(Parameter(0,2)))
    printfn "%A" (Graph.gradientForGroup 0 g)

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code


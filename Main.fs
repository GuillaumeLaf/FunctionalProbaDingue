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

    // let g = Input(Parameter(0,0)) + Input(Parameter(0,1)) * (Input(Parameter(0,1))*Input(Parameter(0,2)) + Input(Parameter(1,2)))
    let g = Model.ModelGraph.create (VAR({n=2; order=2; parameters=None; covariance=None}))
    printfn "%A" (Array.map (Graph.shift Variable 1) g)
    
    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code


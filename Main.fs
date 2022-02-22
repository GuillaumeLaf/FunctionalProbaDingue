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
    let dgp = (VAR({n=1; order=1; parameters=Some([|Array2D.ofArray [|[|0.7f|]|]|]); covariance=Some(Array2D.ofArray [|[|1.0f|]|])}))
    let m = Model.create dgp
    let sampleModel = Model.sample 100 m
    let fitted = Model.fit sampleModel (Optimisation.Optimizer.Classic(0.9f))
    printfn "%A" fitted
    
    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code


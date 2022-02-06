
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


    //printfn "%A" (Graph.add (Input(Parameter(0,0))) (Input(Parameter(0,1))))

    // let parameters = (Utils.Array2D.ofArray >> Array.singleton) [|[|0.25f; 0.0f|]; [|0.6f; 0.45f|]|]
    let p = [|[|0.25f; 0.0f|]; [|0.6f; 0.45f|]|]
    let parameters = [| Array2D.init 2 2 (fun i j -> p.[i].[j]) |]
    // let innovCovariance = Utils.Array2D.ofArray [|[|1.0f;0.6f|];[|0.6f;1.0f|]|]
    let inn = [|[|1.0f;0.6f|];[|0.6f;1.0f|]|]
    let innovCovariance = Array2D.init 2 2 (fun i j -> inn.[i].[j])
    let m = VAR({n=2; order=1; parameters=Some parameters; covariance=Some innovCovariance})
    let model = Model.create m
    
    let sampledModel = Model.sample 1000 model
    printfn "%A" sampledModel
    //let data = sampledModel.Ts.Value.Data.[0,*] |> Array.map float

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code


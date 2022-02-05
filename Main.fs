
open System
open FSharpPlus.Data
open FSharpPlus
open Models
open Models.ModelType


[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let parameters = (Array2D.ofArray >> Array.singleton) [|[|0.25f; 0.0f|]; [|0.6f; 0.45f|]|]
    let innovCovariance = Array2D.ofArray [|[|1.0f;0.0f|];[|0.0f;1.0f|]|]
    let m = VAR({n=2; order=1; parameters=Some parameters; covariance=Some innovCovariance})
    let model = Model.create m
    
    let sampledModel = Model.sample 1000 model
    printfn "%A" sampledModel
    let data = sampledModel.Ts.Value.data.[0,*] |> Array.map float

    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code


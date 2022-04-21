open System
open FSharpPlus.Data
open FSharpPlus
open ComputationalGraph
open ComputationalGraph.GraphType
open Models
open Models.ModelType
open Utils
open Database
open Timeseries
open Timeseries.TimeseriesType

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let n = 100

    let data = [|[| for i in 0..n -> float32 i |]; [| for i in 10..n+10 -> float32 i |]|] |> Array2D.ofArray
    let ts = TS.create data

    let transfo = [TotalDifference(None)]

    let transfoTS = Transformations.forward transfo ts
    printfn "%A" transfoTS

    let transfoBack = Transformations.backward transfoTS
    printfn "%A" transfoBack

(*    // let g = Input(Parameter(0,0)) + Input(Parameter(0,1)) * (Input(Parameter(0,1))*Input(Parameter(0,2)) + Input(Parameter(1,2)))
    let dgp = VAR({n=2; order=1; parameters=Some([|(Array2D.ofArray >> Array2D.toOption) [|[|0.7f;-0.2f|];[|0.1f;-0.5f|]|]|]); covariance=Some(Array2D.ofArray [|[|1.0f;0.0f|];[|0.0f;1.0f|]|])})
    let m = Model.create dgp
    let sampleModel = Model.sample 1000 m

    let fitted = Model.fit sampleModel (Optimisation.Optimizer.Momentum(0.005f, 0.9f)) 100
    printfn "%A" fitted*)

(*    let data = Database.DB.Table.Timeseries.closePrices [|"1INCHBTC";"1INCHBUSD";"1INCHDOWNUSDT"|] (new DateTime(2021,1,1)|> Some) (new DateTime(2021,11,5)|> Some)
    let data = data |> Array.removeManyAt 0 (Array.length data-1000) |> Array.transpose |> Array2D.ofArray |> Array2D.map (Option.defaultValue 0.0 >> unbox<float> >> float32)  
    
    let dgp = VAR({n=3; order=1; parameters=None; covariance=None})
    let ts = Timeseries.TimeseriesType.TS.create data*)

(*    let model = (Model.create >> Model.setTs (Some ts)) dgp
    let fitted = Model.fit model (Optimisation.Optimizer.Momentum(0.005f, 0.9f)) 100
    printfn "%A" fitted*)
  
    //Database.DB.Table.Timeseries.closePrices [|"1INCHBTC";"1INCHBUSD";"1INCHDOWNUSDT"|] (new DateTime(2021,1,1)|> Some) (new DateTime(2021,11,5)|> Some) |> printfn "%A"



    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code


open System
open FSharpPlus.Data
open FSharpPlus
open ComputationalGraph
open ComputationalGraph.GraphType
open Models
open ModelType
open Model_Selection
open Utils
// open Database
open Timeseries
open Timeseries.TimeseriesType
open Plot

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()


(*    let data = [|[|None; Some 1f; Some 2f ; None; Some 3f; None|]; [|Some 1f; Some 1f; Some 2f ; None; None; Some 4f|]|] |> Array2D.ofArray
    let ts = Timeseries.TimeseriesType.TS.create ([|[|0f|]|] |> Array2D.ofArray)
    let ts  = TS.setData data ts

    let transfo = [DefaultWith(1f,None); Apply(log, exp)]
    let transfoTS = Transformations.forward transfo ts
    let transfoTS = Transformations.backward transfoTS
    printfn "%A" transfoTS*)

(*    let g = Input(Parameter(0,0)) + Input(Parameter(0,1)) * (Input(Parameter(0,1))*Input(Parameter(0,2)) + Input(Parameter(1,2)))
    printfn "%A" (Graph.collectInputs g)*)

    let dgp = VAR({n=2; order=1; parameters=Some([|(Array2D.ofArray >> Array2D.toOption) [|[|0.7f;-0.2f|];[|0.1f;-0.5f|]|]|]); covariance=Some(Array2D.ofArray [|[|1.0f;0.0f|];[|0.0f;1.0f|]|])})
    let m = Model.create dgp
    let m, sample, _ = Model.sample 1000 m

    printfn "%A" sample

    //let fitFunc = Model.fit (Optimisation.Optimizer.Momentum(0.005f, 0.9f)) (L2Regu(0.2f)) 5
    let fitFunc = Model.fit (Optimisation.Optimizer.Momentum(0.005f, 0.9f)) SquaredError 25

    let OOSerrors = CrossValidation.errors (CrossValidation.Rolling) 5 fitFunc sample m
    printfn "%A" OOSerrors

(*    let fittedm,_,_,ts = Model.fit m (Optimisation.Optimizer.Momentum(0.005f, 0.9f)) (L2Regu(0.2f)) 100 sample
    printfn "%A" (Model.multiPredict fittedm 10 ts)*)

(*    let data = Database.DB.Table.Timeseries.closePrices [|"1INCHBTC";"1INCHBUSD";"1INCHDOWNUSDT"|] (new DateTime(2021,1,1)|> Some) (new DateTime(2021,11,5)|> Some)
    // let data = data |> Array.removeManyAt 0 (Array.length data-1000) |> Array.transpose |> Array2D.ofArray |> Array2D.map (Option.defaultValue 0.0 >> unbox<float> >> float32)  
    let data = data |> Array.removeManyAt 0 (Array.length data-1000) |> Array.transpose |> Array2D.ofArray |> Array2D.map (Option.map (unbox<float> >> float32))
    // let data = Array2D.map ((+) 10f) data

    let dgp = VAR({n=3; order=1; parameters=None; covariance=None})
    // let ts = Timeseries.TimeseriesType.TS.create data
    let ts = Timeseries.TS.zeroCreate 1 1
    let ts = Timeseries.TimeseriesType.TS.setData data ts

    // Plot.allSeries ts

    let transfo = [Apply(log, exp); TotalDifference(None); Center(None); Standardize(None)]
    let transfoTS = Transformations.forward transfo ts
    printfn "%A" transfoTS

    Plot.allSeries transfoTS

    let model = (Model.create >> Model.setTs (Some transfoTS)) dgp
    let fitted = Model.fit model (Optimisation.Optimizer.Momentum(0.005f, 0.9f)) (L2Regu(0.2f)) 100
    printfn "%A" fitted*)
  
    //Database.DB.Table.Timeseries.closePrices [|"1INCHBTC";"1INCHBUSD";"1INCHDOWNUSDT"|] (new DateTime(2021,1,1)|> Some) (new DateTime(2021,11,5)|> Some) |> printfn "%A"



    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code


﻿open System
open FSharpPlus.Data
open FSharpPlus
open ComputationalGraph
open ComputationalGraph.GraphType
open Models
open Models.ModelType
open Utils
open Database

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    // let g = Input(Parameter(0,0)) + Input(Parameter(0,1)) * (Input(Parameter(0,1))*Input(Parameter(0,2)) + Input(Parameter(1,2)))
(*    let dgp = (VAR({n=2; order=1; parameters=Some([|Array2D.ofArray [|[|0.7f;-0.2f|];[|0.1f;-0.5f|]|]|]); covariance=Some(Array2D.ofArray [|[|1.0f;0.0f|];[|0.0f;1.0f|]|])}))
    let m = Model.create dgp
    let sampleModel = Model.sample 1000 m
    let fitted = Model.fit sampleModel (Optimisation.Optimizer.Momentum(0.005f, 0.9f)) 100
    printfn "%A" fitted*)

    //Database.DB.Table.Timeseries.Query.closePrices [|"1INCHBTC";"1INCHBUSD";"1INCHDOWNUSDT"|] (new DateTime(2021,1,1)) (new DateTime(2021,11,5)) |> printfn "%A"
    //Database.DB.Table.Timeseries.Query.closePrices [|"1INCHBTC";"1INCHBUSD";"1INCHDOWNUSDT"|] (new DateTime(2021,1,1)) (new DateTime(2021,11,5)) |> (Seq.toArray >> printfn "%A")    
  
    Database.DB.Table.Timeseries.closePrices [|"1INCHBTC";"1INCHBUSD";"1INCHDOWNUSDT"|] (new DateTime(2021,1,1)|> Some) (new DateTime(2021,11,5)|> Some)



    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code


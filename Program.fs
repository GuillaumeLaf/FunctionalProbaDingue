// Learn more about F# at http://fsharp.org

open System
open TimeSeries
open Models
open Monads
open FSharp.Charting
open Binance

open DataBase

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    // DB.importFromAggregate (Helper.Crypto("BTCUSDT", Helper.M15))
    (*Helper.symbolsInAggregate Helper.M15 
        |> Array.iter (fun x -> DB.importFromAggregate (Helper.Crypto(x, Helper.M15)) DB.Real)*)
(*    [|Helper.Crypto("BTCNGN", Helper.M15);Helper.Crypto("BTCRUB", Helper.M15);
        Helper.Crypto("BTCUAH", Helper.M15)|] |> Array.map(fun x -> DB.importFromAggregate x)
        |> ignore*)

(*    let startTime = new DateTime(2021,6,30)
    let endTime = new DateTime(2021,10,31)*)
    //Downloader.download [|"AAVEBTC";"AAVEBUSD";"AAVEDOWNUSDT";"AAVEETH";"AAVEUPUSDT";"AAVEUSDT";"ACMBTC";"ACMBUSD";"ACMUSDT";"ADAAUD";"ADABIDR";"ADABKRW"|] startTime endTime
    //Downloader.downloadAll startTime endTime

    //Downloader.downloadAll (Helper.M15) startTime endTime
    //Aggregator.aggregateAll (Helper.M15)

    //Downloader.download Helper.M15 startTime endTime


    
    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code
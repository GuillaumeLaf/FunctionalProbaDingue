// Learn more about F# at http://fsharp.org

open System
open TimeSeries
open Models
open Monads
open FSharp.Charting
open Binance

open Binance.Net

open System.IO
open System

[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let endTime = new DateTime(2021,10,21) |> Nullable
    let d = Downloader.downloadOne (Downloader.Crypto("RCNETH", Downloader.M15)) endTime
                |> Async.RunSynchronously

(*    let client = new BinanceClient()

    let startTime = new DateTime(2021,10,18)
    let endTime = new DateTime(2021,10,20)*)

(*    let result = client.Spot.System.GetExchangeInfoAsync()
                    |> Async.AwaitTask
                    |> Async.RunSynchronously
    printfn "%A" (result.Data.Symbols |> Seq.map (fun x -> x.Name)
                                      |> Seq.length)*)
(*    let result = client.Spot.Market.GetKlinesAsync("BNBBTC",Enums.KlineInterval.FifteenMinutes,startTime,endTime)
                    |> Async.AwaitTask
                    |> Async.RunSynchronously
    printfn "%A" (result.Data |> Seq.map (fun (x:Binance.Net.Interfaces.IBinanceKline) -> x.CloseTime)
                              |> Seq.length)*)
    
    stopWatch.Stop()
    printfn "%f seconds elapsed" (stopWatch.Elapsed.TotalMilliseconds / 1000.0)

    0 // return an integer exit code
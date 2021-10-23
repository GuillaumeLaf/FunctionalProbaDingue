namespace Binance

open System
open System.Linq
open System.IO
open Binance.Net

module Helper = 
    let path = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\Binance"
    let pathDL = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\NewData"
    let client = new BinanceClient()

    type PathType = 
        | Download
        | Aggregate

    type Interval = 
        | M15

    type Crypto = Crypto of string * Interval

    let intervalToString = function
        | M15 -> "M15"

    let intervalToBinanceInterval = function
        | M15 -> Enums.KlineInterval.FifteenMinutes

    let getPath = function
        | Download -> pathDL
        | Aggregate -> path

    let getCryptoPath (Crypto(cr,interval)) = getPath >> (fun path -> Path.Combine(path, intervalToString interval, cr + ".csv"))

(*    let getLastData t crypto = 
        match (doesCryptoExists crypto) with
        | true -> File.ReadLines(getPath crypto).Last()
        | false -> ""*)

    let getLastTime t crypto = 
        try 
            let previousData = File.ReadAllLines(getCryptoPath crypto t).Last().Split [|';'|]
            let previousOpenTime = previousData.Last()
            let splittedTime = previousOpenTime.Split [|' '|]
            let date = splittedTime.[0].Split [|'/'|]
            let time = splittedTime.[1].Split [|':'|]
            new DateTime(int date.[2],int date.[0],int date.[1],int time.[0],int time.[1],int time.[2])
        with
        | :? System.IO.FileNotFoundException -> new DateTime(2021,10,20)


module Downloader =
    // Define when was the last time we got data.
    // Download the needed data for the needed cryptos.

    let getLastTime = Helper.getLastTime Helper.Download

    let doesIntervalExists interval = Path.Combine(Helper.getPath Helper.Download,(Helper.intervalToString interval)) |> File.Exists
    let doesCryptoExists crypto = Helper.getCryptoPath crypto Helper.Download |> File.Exists

    let downloadOne crypto endTime =  // First need to get the startTime (which should be an option ?)
        let (Helper.Crypto(cr,interval)) = crypto
        let startTime = Helper.getLastTime Helper.Aggregate crypto  // Get the last time we updated the database files. 
        async{
            let! rawData = Helper.client.Spot.Market.GetKlinesAsync(cr,Helper.intervalToBinanceInterval interval,startTime,endTime) 
                            |> Async.AwaitTask
            let klinesData = rawData.Data
            let firstLine = "CloseTime;Open;High;Low;Close;QuoteVolume;BaseVolume;TradeCount;OpenTime"
            let klinesData = klinesData |> Seq.map (fun x -> string x.CloseTime + ";" + string x.Open + ";" + string x.High + ";" +
                                                             string x.Low + ";" + string x.Close + ";" + string x.QuoteVolume + ";" + 
                                                             string x.BaseVolume + ";" + string x.TradeCount + ";" + string x.OpenTime)
            File.WriteAllLines(Helper.getCryptoPath crypto Helper.Download, seq{yield firstLine; yield! klinesData})
        }

    let updateSymbols() = 
        let symbols = 
            async{
                let! result = Helper.client.Spot.System.GetExchangeInfoAsync() |> Async.AwaitTask
                return result.Data.Symbols |> Seq.map (fun x -> x.Name)
            } |> Async.RunSynchronously
        File.WriteAllLines(Helper.pathDL+"\Symbols.csv",symbols)

    let download cryptos endTime = 0

module Aggregator =    // The essence of this module is to aggregate the downloaded files with the data base files. 
    let getLastTime = Helper.getLastTime Helper.Aggregate

    let doesIntervalExists interval = Path.Combine(Helper.getPath Helper.Aggregate,(Helper.intervalToString interval)) |> File.Exists
    let doesCryptoExists crypto = Helper.getCryptoPath crypto Helper.Aggregate |> File.Exists
        
    // Find a way to compare the last time in the database file with the first time in the download file (-> avoid duplicates). 
    // Then append the correct data at the end of the database file. 
    
        

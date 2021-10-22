namespace Binance

open System
open System.Linq
open System.IO
open Binance.Net

module Collector =
    // Define when was the last time we got data.
    // Download the needed data for the needed cryptos.
    let path = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\Binance"
    let pathDL = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\NewData"
    let client = new BinanceClient()

    type Interval = 
        | M15

    type Crypto = Crypto of string * Interval


    let intervalToString = function
        | M15 -> "M15"

    let intervalToBinanceInterval = function
        | M15 -> Enums.KlineInterval.FifteenMinutes

    let getPath (Crypto(cr,interval)) = Path.Combine(path, intervalToString interval, cr + ".csv")
    let getPathDL (Crypto(cr,interval)) = Path.Combine(pathDL, intervalToString interval, cr + ".csv")

    let doesIntervalExists interval = Path.Combine(path,(intervalToString interval)) |> File.Exists
    let doesCryptoExists = getPath >> File.Exists

    let getLastData crypto = 
        match (doesCryptoExists crypto) with
        | true -> File.ReadLines(getPath crypto).Last()
        | false -> ""

    let getLastTime crypto = 
        try 
            let previousData = File.ReadAllLines(getPath crypto).Last().Split [|';'|]
            let previousOpenTime = previousData.Last()
            let splittedTime = previousOpenTime.Split [|' '|]
            let date = splittedTime.[0].Split [|'/'|]
            let time = splittedTime.[1].Split [|':'|]
            new DateTime(int date.[2],int date.[0],int date.[1],int time.[0],int time.[1],int time.[2])
        with
        | :? System.IO.FileNotFoundException -> new DateTime(2021,10,20)

    let downloadOne crypto endTime =  // First need to get the startTime (which should be an option ?)
        let (Crypto(cr,interval)) = crypto
        let startTime = getLastTime crypto 
        async{
            let! rawData = client.Spot.Market.GetKlinesAsync(cr,intervalToBinanceInterval interval,startTime,endTime) 
                            |> Async.AwaitTask
            let klinesData = rawData.Data
            let firstLine = "CloseTime;Open;High;Low;Close;QuoteVolume;BaseVolume;TradeCount;OpenTime"
            let klinesData = klinesData |> Seq.map (fun x -> string x.CloseTime + ";" + string x.Open + ";" + string x.High + ";" +
                                                             string x.Low + ";" + string x.Close + ";" + string x.QuoteVolume + ";" + 
                                                             string x.BaseVolume + ";" + string x.TradeCount + ";" + string x.OpenTime)
            File.WriteAllLines(getPathDL crypto, seq{yield firstLine; yield! klinesData})
        }

    let updateSymbols() = 
        let symbols = 
            async{
                let! result = client.Spot.System.GetExchangeInfoAsync() |> Async.AwaitTask
                return result.Data.Symbols |> Seq.map (fun x -> x.Name)
            } |> Async.RunSynchronously
        File.WriteAllLines(pathDL+"\Symbols.csv",symbols)

    let download cryptos endTime = 0
        
            
        
     
        

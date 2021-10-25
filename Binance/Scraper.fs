namespace Binance

open System
open System.Linq
open System.IO
open Binance.Net

module Helper = 
    let path = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\Binance"
    let pathDL = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\NewData"
    let header = "CloseTime;Open;High;Low;Close;QuoteVolume;BaseVolume;TradeCount;OpenTime"
    let client = new BinanceClient()

    type PathType = 
        | Download
        | Aggregate

    type Interval = 
        | M15

    type Crypto = Crypto of string * Interval

    type Data = 
        | CloseTime
        | Open
        | High
        | Low
        | Close
        | QuoteVolume
        | BaseVolume
        | TradeCount
        | OpenTime

    let intervalToString = function
        | M15 -> "M15"

    let intervalToBinanceInterval = function
        | M15 -> Enums.KlineInterval.FifteenMinutes

    let getPath = function
        | Download -> pathDL
        | Aggregate -> path

    let cryptoName (Crypto(name,_)) = name

    let getCryptoPath (Crypto(cr,interval)) = getPath >> (fun path -> Path.Combine(path, intervalToString interval, cr + ".csv"))
    let cryptoExists t crypto = getCryptoPath crypto t |> File.Exists

    let getSymbols() = File.ReadAllLines(Path.Combine(pathDL, "Symbols.csv"))
    let isHeader (row:string) = row.[0..4] = "Close"

    let getLastTime t crypto = 
        if (cryptoExists t crypto) then
            let previousData = File.ReadAllLines(getCryptoPath crypto t).Last()
            if isHeader previousData then 
                new DateTime(2021,10,18)
            else
                let previousData = previousData.Split [|';'|]
                let previousOpenTime = previousData.Last()
                let splittedTime = previousOpenTime.Split [|' '|]
                let date = splittedTime.[0].Split [|'/'|]
                let time = splittedTime.[1].Split [|':'|]
                new DateTime(int date.[2],int date.[0],int date.[1],int time.[0],int time.[1],int time.[2])
        else
            new DateTime(2021,10,18)

    let extractOpenTime (s:string) = (s.Split [|';'|]).Last()
    
    let parseDate (s:string) = 
        let splittedTime = s.Split [|' '|]
        let date = splittedTime.[0].Split [|'/'|]
        let time = splittedTime.[1].Split [|':'|]
        new DateTime(int date.[2],int date.[0],int date.[1],int time.[0],int time.[1],int time.[2])


module Downloader =

    let getLastTime = Helper.getLastTime Helper.Download

    let doesIntervalExists interval = Path.Combine(Helper.getPath Helper.Download,(Helper.intervalToString interval)) |> File.Exists
    let doesCryptoExists crypto = Helper.getCryptoPath crypto Helper.Download |> File.Exists

    let updateSymbols() = 
        let symbols = 
            async{
                let! result = Helper.client.Spot.System.GetExchangeInfoAsync() |> Async.AwaitTask
                return result.Data.Symbols |> Seq.map (fun x -> x.Name)
            } |> Async.RunSynchronously
              |> Seq.sort
        File.WriteAllLines(Helper.pathDL+"\Symbols.csv",symbols)

    let checkAPILimits (caller:CryptoExchange.Net.Objects.WebCallResult<'T>) = 
        async{
            let currentW = BinanceHelpers.UsedWeight(caller.ResponseHeaders).Value
            printfn "%i" currentW
            if  currentW >= 1000 then 
                do! Async.Sleep(60000)
        }
       
    let downloadOne crypto startTime endTime =  // First need to get the startTime (which should be an option ?)
        let (Helper.Crypto(cr,interval)) = crypto
        async{
            printfn "%s" ("Downloading " + cr)
            let! rawData = Helper.client.Spot.Market.GetKlinesAsync(cr,Helper.intervalToBinanceInterval interval,startTime,endTime) 
                            |> Async.AwaitTask
            do! checkAPILimits rawData
            let klinesData = rawData.Data |> Seq.map (fun x -> string x.CloseTime + ";" + string x.Open + ";" + string x.High + ";" +
                                                               string x.Low + ";" + string x.Close + ";" + string x.QuoteVolume + ";" + 
                                                               string x.BaseVolume + ";" + string x.TradeCount + ";" + string x.OpenTime)
            File.WriteAllLines(Helper.getCryptoPath crypto Helper.Download, seq{yield Helper.header; yield! klinesData})
        }

    let downloadAll startTime endTime = 
        Helper.getSymbols() |> Seq.map (fun x -> downloadOne (Helper.Crypto(x,Helper.M15)) startTime endTime)
                            |> (fun x -> Async.Parallel (x,5))
                            |> Async.RunSynchronously
                            |> ignore

    let download cryptoNames startTime endTime = 
        cryptoNames |> Array.map (fun x -> downloadOne (Helper.Crypto(x,Helper.M15)) startTime endTime)
                    |> Async.Parallel
                    |> Async.RunSynchronously
                    |> ignore
        
module Aggregator =    // The essence of this module is to aggregate the downloaded files with the data base files. 
    let getLastTime = Helper.getLastTime Helper.Aggregate

    let doesIntervalExists interval = Path.Combine(Helper.getPath Helper.Aggregate,(Helper.intervalToString interval)) |> File.Exists
    let doesCryptoExists = Helper.cryptoExists Helper.Aggregate

    let compareOpenTimes (dbTime:DateTime) dlTime = if (dlTime - dbTime).TotalDays > 0.0 then true else false
        
    let isRowValidForAggregation dbTime (rowString:string) = 
        rowString |> Helper.extractOpenTime
                  |> Helper.parseDate
                  |> compareOpenTimes dbTime

    let checkHeader crypto sq = 
        match (doesCryptoExists crypto) with
            | true -> sq
            | false -> seq {yield Helper.header; yield! sq}

    let aggregateOne crypto = 
        async{
            printfn "%s" ("aggregating " + (Helper.cryptoName crypto))
            let lastDataBaseTime = getLastTime crypto
            File.ReadAllLines(Helper.getCryptoPath crypto Helper.Download)
                |> Seq.skip 1
                |> Seq.filter(fun x -> x |> isRowValidForAggregation lastDataBaseTime)
                |> checkHeader crypto
                |> (fun newLines -> File.AppendAllLines(Helper.getCryptoPath crypto Helper.Aggregate, newLines))
        }

    let aggregateAll () = 
        Helper.getSymbols() |> Seq.map (fun x -> aggregateOne (Helper.Crypto(x,Helper.M15)))
                            |> (fun x -> Async.Parallel (x,5))
                            |> Async.RunSynchronously
                            |> ignore

    // Find a way to compare the last time in the database file with the first time in the download file (-> avoid duplicates). 
    // Then append the correct data at the end of the database file. 
    
        

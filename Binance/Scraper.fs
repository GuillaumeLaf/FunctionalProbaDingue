namespace Binance

open System
open System.Linq
open System.IO
open Binance.Net
open Monads

module Helper = 
    let path = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\Binance"
    let pathDL = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\NewData"
    let header = "CloseTime;Open;High;Low;Close;QuoteVolume;BaseVolume;TradeCount;OpenTime"
    let defaultTime = new DateTime(2021,10,18)
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

    let cryptoName (Crypto(name,_)) = name

    let cryptoPath (Crypto(cr,interval)) = getPath >> (fun path -> Path.Combine(path, intervalToString interval, cr + ".csv"))

    let intervalPath interval = getPath >> (fun path -> Path.Combine(path, intervalToString interval))

    let symbolsList = File.ReadAllLines(Path.Combine(pathDL, "Symbols.csv"))
    let isHeader (row:string) = row.[0..4] = "Close"

    let checkExistence filePath = 
        match (File.Exists filePath) || (Directory.Exists filePath) with
        | true -> Some filePath
        | false -> None

    let cryptoData crypto = cryptoPath crypto >> checkExistence >> Option.map File.ReadAllLines
    let lastData crypto = cryptoData crypto >> Option.map Seq.last
    let _lastOpenTime crypto = lastData crypto >> Option.map (fun s -> s.Split ';') >> Option.map Seq.last

    let stringToDateTime (s:string) = 
        let splitted = s.Split ' '
        let date = splitted.[0].Split '/'
        let time = splitted.[1].Split ':'
        new DateTime(int date.[2], int date.[0], int date.[1], int time.[0], int time.[1], int time.[2])

    let lastDateTime crypto = _lastOpenTime crypto >> Option.map stringToDateTime

    let extractOpenTime (s:string) = s.Split ';' |> Seq.last 
   

module Aggregator =   
    let (<!>) = Option.map
    let (>>=) x f = Option.bind f x

    let lastTime crypto = Helper.lastDateTime crypto Helper.Aggregate
    let cryptoData crypto = Helper.cryptoData crypto Helper.Aggregate
    let cryptoDataDL crypto = Helper.cryptoData crypto Helper.Download
    let cryptoPath crypto = Helper.cryptoPath crypto Helper.Aggregate

    let compareOpenTimes (dbTime:DateTime) dlTime = if (dlTime - dbTime).TotalDays > 0.0 then true else false

    let isRowValidForAggregation dbTime = Helper.extractOpenTime >> Helper.stringToDateTime >> compareOpenTimes dbTime

    let addHeaderIfNeeded crypto sq = 
        cryptoPath crypto |> (Helper.checkExistence >> Option.isSome)
                          |> (fun b -> if b then sq else seq{yield Helper.header; yield! sq})
    
    let aggregateOne dbTime crypto = 
        cryptoDataDL crypto |> Option.map (Seq.skip 1)
                            |> Option.map (Seq.filter (fun x -> isRowValidForAggregation dbTime x))
                            |> Option.map (addHeaderIfNeeded crypto) 
                            |> Option.map (fun newLines -> File.AppendAllLines(cryptoPath crypto, newLines))

    let aggregateOneM () = aggregateOne <!> Helper.cryptoDataM Helper.Download <*> Helper.lastTimeM Helper.Aggregate
                                        <*> Helper.cryptoNameM <*> Helper.cryptoIntervalM

    let aggregateAll interval = 
        let result = Helper.symbolsList |> Array.map (fun s -> Monad.state{do! Helper.updateSymbolM s
                                                                           return! aggregateOneM()})
                                        |> Monad.mapM   
                                        >>= (fun xs -> (xs,5) |> Async.Parallel |> Async.RunSynchronously |> Monad.rets)
        Monad.run result (Helper.defaultState interval) |> ignore

module Downloader =
    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x

    let lastTimeM () = Helper.lastTimeM Helper.Download
    let intevalExistsM () = Helper.intervalExistsM Helper.Download
    let cryptoExistsM () = Helper.cryptoExistsM Helper.Download

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

    let downloadOne (startTime:DateTime) (endTime:DateTime) crypto interval =  // First need to get the startTime (which should be an option ?)
        async{
            printfn "%s" ("Downloading " + crypto)
            let! rawData = Helper.client.Spot.Market.GetKlinesAsync(crypto,Helper.intervalToBinanceInterval interval,startTime,endTime) 
                            |> Async.AwaitTask
            do! checkAPILimits rawData
            let klinesData = rawData.Data |> Seq.map (fun x -> string x.CloseTime + ";" + string x.Open + ";" + string x.High + ";" +
                                                               string x.Low + ";" + string x.Close + ";" + string x.QuoteVolume + ";" + 
                                                               string x.BaseVolume + ";" + string x.TradeCount + ";" + string x.OpenTime)
            let path = Helper.getCryptoPath Helper.Download crypto interval
            File.WriteAllLines(path, seq{yield Helper.header; yield! klinesData})
        }

    let downloadOneM startTime endTime = downloadOne startTime endTime <!> Helper.cryptoNameM <*> Helper.cryptoIntervalM

    let downloadAll interval (startTime:DateTime) (endTime:DateTime) = 
        let result = Helper.symbolsList |> Array.map (fun s -> Monad.state{do! Helper.updateSymbolM s
                                                                           return! downloadOneM startTime endTime})
                                        |> Monad.mapM   
                                        >>= (fun xs -> (xs,5) |> Async.Parallel |> Async.RunSynchronously |> Monad.rets)
        Monad.run result (Helper.defaultState interval) |> ignore

    let download interval (startTime:DateTime) (endTime:DateTime) = 
        let totalTime = endTime - startTime
        let nbWeeks = totalTime.Days / 7
        let r = totalTime.Days - nbWeeks
        let weekIntervals = [| for i in 0..nbWeeks-1 do (startTime.Add(TimeSpan(7*i,0,0,0)), startTime.Add(TimeSpan(7*(i+1),0,0,0))) |]
        let weekIntervals = Array.concat [|weekIntervals ; [|(endTime.Subtract(TimeSpan(r,0,0,0)),endTime)|]|]
        weekIntervals |> Array.map (fun (s,e) -> downloadAll interval s e
                                                 Aggregator.aggregateAll interval)
                      |> ignore


(*    let download cryptoNames startTime endTime = 
        cryptoNames |> Array.map (fun x -> downloadOne (Helper.Crypto(x,Helper.M15)) startTime endTime)
                    |> Async.Parallel
                    |> Async.RunSynchronously
                    |> ignore*)
        


    
    

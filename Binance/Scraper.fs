namespace Binance

open System
open System.Linq
open System.IO
open Binance.Net
open Monads

module Helper = 
    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x

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

    type Crypto = Crypto of string

    type State<'T> = State of Crypto * Interval

    let intervalToString = function
        | M15 -> "M15"

    let intervalToBinanceInterval = function
        | M15 -> Enums.KlineInterval.FifteenMinutes

    let getPath = function
        | Download -> pathDL
        | Aggregate -> path

    let cryptoNameM = Monad.M (fun (State(Crypto(name),interval)) -> name,State(Crypto(name),interval))
    let cryptoIntervalM = Monad.M (fun (State(Crypto(name),interval)) -> interval,State(Crypto(name),interval))

    let getCryptoPath t name interval = Path.Combine(getPath t, intervalToString interval, name + ".csv")
    let cryptoPathM t = getCryptoPath t <!> cryptoNameM <*> cryptoIntervalM
    let cryptoExistsM t = File.Exists <!> cryptoPathM t

    let getIntervalpath t interval = Path.Combine(getPath t, intervalToString interval)
    let intervalPathM t = getIntervalpath t <!> cryptoIntervalM
    let intervalExistsM t = Directory.Exists <!> intervalPathM t

    let symbolsListM = File.ReadAllLines(Path.Combine(pathDL, "Symbols.csv")) |> Monad.rets

    let isHeader (row:string) = row.[0..4] = "Close"

    let cryptoDataM t = File.ReadAllLines <!> cryptoPathM t

    let fileDefaultValue t defaultValue fileM = 
        Monad.state{
            let! doesExists = cryptoExistsM t
            if doesExists then 
                return! fileM
            else return defaultValue
        }

    let lastTimeM t =
        Monad.state{
            let! lastDataRow = Seq.last <!> cryptoDataM t
            if isHeader lastDataRow then 
                return defaultTime
            else
                let lastDataRow = lastDataRow.Split [|';'|]
                let lastOpenTime = lastDataRow.Last()
                let splittedTime = lastOpenTime.Split [|' '|]
                let date = splittedTime.[0].Split [|'/'|]
                let time = splittedTime.[1].Split [|':'|]
                return new DateTime(int date.[2],int date.[0],int date.[1],int time.[0],int time.[1],int time.[2])
        } |> fileDefaultValue t defaultTime

    let extractOpenTime (s:string) = (s.Split [|';'|]).Last()
    
    let parseDate (s:string) = 
        let splittedTime = s.Split [|' '|]
        let date = splittedTime.[0].Split [|'/'|]
        let time = splittedTime.[1].Split [|':'|]
        new DateTime(int date.[2],int date.[0],int date.[1],int time.[0],int time.[1],int time.[2])


module Downloader =
    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x

    let lastTimeM = Helper.lastTimeM Helper.Download
    let intevalExistsM = Helper.intervalExistsM Helper.Download
    let cryptoExistsM = Helper.cryptoExistsM Helper.Download

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

    let downloadOne startTime endTime crypto interval =  // First need to get the startTime (which should be an option ?)
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
    
        

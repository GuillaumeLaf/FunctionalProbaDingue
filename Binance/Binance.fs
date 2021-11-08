namespace Binance

open System
open System.Linq
open System.IO
open System.Threading
open Binance.Net
open Monads

module Helper = 
    let path = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\Binance"
    let pathDL = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\NewData"
    let header = "CloseTime;Open;High;Low;Close;QuoteVolume;BaseVolume;TradeCount;OpenTime"
    let defaultTime = new DateTime(2000,10,1)
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

    let symbolsInAggregate interval = Directory.GetFiles(intervalPath interval Aggregate, "*.csv") 
                                        |> Array.map (Path.GetFileName >> (fun s -> s.[0..s.Length-5]))

    let symbolsList = File.ReadAllLines(Path.Combine(pathDL, "Symbols.csv"))
    let isHeader (row:string) = row.[0..4] = "Close"

    let checkExistence filePath = 
        match (File.Exists filePath) || (Directory.Exists filePath) with
        | true -> Some filePath
        | false -> None

    let rec checkIfInUse filePath = 
        async{
            try 
                use stream = File.Open(filePath, FileMode.Open)
                stream.Close()
                return filePath
            with
            | :? System.IO.IOException -> do! Async.Sleep(500)
                                          return! (checkIfInUse filePath)
        }

    let cryptoData crypto t = 
        async{
            let path = cryptoPath crypto t |> checkExistence
            let path = path |> Option.map checkIfInUse |> Option.map Async.RunSynchronously
            return Option.map File.ReadAllLines path
        }
    
    let lastData crypto t = 
        async{
            let! data = cryptoData crypto t
            let last = data |> Option.map Seq.last
            return last |> Option.bind (fun x -> if isHeader x then None else Some x) // if the file only contains the header. it is considered empty.
        }

    let _lastOpenTime crypto t = 
        async {
            let! lastData = lastData crypto t
            return (lastData |> Option.map (fun s -> s.Split ';') |> Option.map Seq.last)
        }   

    let _firstOpenTime crypto t = 
        async{
            let! data = cryptoData crypto t
            return (data |> Option.map (Seq.item 1) |> Option.map (fun s -> s.Split ';') |> Option.map Seq.last)
        }

    let stringToDateTime (s:string) = 
        let splitted = s.Split ' '
        let date = splitted.[0].Split '/'
        let time = splitted.[1].Split ':'
        new DateTime(int date.[2], int date.[0], int date.[1], int time.[0], int time.[1], int time.[2])

    let lastDateTime crypto t = 
        async{
            let! lastOpen = _lastOpenTime crypto t
            return lastOpen |> Option.map stringToDateTime
        }

    let firstDateTime crypto t = 
        async{
            let! firstOpen =_firstOpenTime crypto t
            return firstOpen |> Option.map stringToDateTime
        }

    let extractOpenTime (s:string) = s.Split ';' |> Seq.last 
   

module Aggregator =   
    let (<!>) = Option.map
    let (>>=) x f = Option.bind f x

    let lastTime crypto = Helper.lastDateTime crypto Helper.Aggregate
    let firstTime crypto = Helper.firstDateTime crypto Helper.Aggregate
    let cryptoData crypto = Helper.cryptoData crypto Helper.Aggregate
    let cryptoDataDL crypto = Helper.cryptoData crypto Helper.Download
    let cryptoPath crypto = Helper.cryptoPath crypto Helper.Aggregate

    let compareOpenTimes (firstDbTime:DateTime) (lastDbTime:DateTime) dlTime = if (dlTime - lastDbTime).TotalDays > 0.0 then true else false // || (dlTime - firstDbTime).TotalDays < 0.0

    let isRowValidForAggregation firstDbTime lastDbTime = Helper.extractOpenTime >> Helper.stringToDateTime >> compareOpenTimes firstDbTime lastDbTime

    let addHeaderIfNeeded crypto sq = 
        cryptoPath crypto |> (Helper.checkExistence >> Option.isSome)
                          |> (fun b -> if b then sq else seq{yield Helper.header; yield! sq})

(*    let concatenateData crypto dataDb dataDL firstDbTime lastDbTime = 
        seq{
            for x in dataDL do
        }

    let writeToDbFile crypto dataDL =
        dataDL |> Option.map (Seq.skip 1)
               |> *)

    let aggregateOne crypto = 
        async{
            let (Helper.Crypto(symbol,interval)) = crypto
            let! dataDL = cryptoDataDL crypto
            let! lastDbTime = lastTime (Helper.Crypto(symbol,interval))
            let! firstDbTime = firstTime (Helper.Crypto(symbol,interval))
            let lastDbTime = lastDbTime |> Option.defaultValue Helper.defaultTime
            let firstDbTime = firstDbTime |> Option.defaultValue Helper.defaultTime
            return (dataDL |> Option.map (Seq.skip 1)
                           |> Option.bind (fun sq -> if Seq.isEmpty sq then None else Some sq) // Empty DL files wont go into the DB. 
                           |> Option.map (Seq.filter (fun x -> isRowValidForAggregation firstDbTime lastDbTime x))
                           |> Option.map (addHeaderIfNeeded crypto) 
                           |> Option.map (fun newLines -> File.AppendAllLines(cryptoPath crypto, newLines)))
        }

    let aggregateAll interval = 
        Helper.symbolsList |> Seq.map (fun symbol -> aggregateOne (Helper.Crypto(symbol,interval)))              
                           |> (fun sq -> Async.Parallel (sq,5))
                           |> Async.RunSynchronously
                           |> ignore

module Downloader =

    let lastTimeDL crypto = Helper.lastDateTime crypto Helper.Download
    let cryptoDataDL crypto = Helper.cryptoData crypto Helper.Download
    let cryptoPathDL crypto = Helper.cryptoPath crypto Helper.Download

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

    let downloadOne (startTime:DateTime) (endTime:DateTime) crypto =  // First need to get the startTime (which should be an option ?)
        async{
            let (Helper.Crypto(cr,interval)) = crypto
            printfn "%s" ("Downloading " + cr)
            let! rawData = Helper.client.Spot.Market.GetKlinesAsync(cr,Helper.intervalToBinanceInterval interval,startTime,endTime) 
                            |> Async.AwaitTask
            do! checkAPILimits rawData
            let klinesData = rawData.Data |> Seq.map (fun x -> string x.CloseTime + ";" + string x.Open + ";" + string x.High + ";" +
                                                               string x.Low + ";" + string x.Close + ";" + string x.QuoteVolume + ";" + 
                                                               string x.BaseVolume + ";" + string x.TradeCount + ";" + string x.OpenTime)
            File.WriteAllLines(cryptoPathDL crypto, seq{yield Helper.header; yield! klinesData})
        }

    let downloadAll interval (startTime:DateTime) (endTime:DateTime) = 
        Helper.symbolsList |> Seq.map (fun symbol -> downloadOne startTime endTime (Helper.Crypto(symbol,interval)))
                           |> (fun sq -> Async.Parallel (sq,5))
                           |> Async.RunSynchronously
                           |> ignore

    let download interval (startTime:DateTime) (endTime:DateTime) = 
        let totalTime = endTime - startTime
        let nbWeeks = totalTime.Days / 7
        let r = totalTime.Days - (nbWeeks*7)
        let weekIntervals = [| for i in 0..nbWeeks-1 do (startTime.Add(TimeSpan(7*i,0,0,0)), startTime.Add(TimeSpan(7*(i+1)-1,0,0,0))) |]
        let weekIntervals = Array.concat [|weekIntervals ; [|(endTime.Subtract(TimeSpan(r,0,0,0)),endTime)|]|]
        weekIntervals |> Array.map (fun (s,e) -> downloadAll interval s e
                                                 Aggregator.aggregateAll interval)
                      |> ignore


(*    let download cryptoNames startTime endTime = 
        cryptoNames |> Array.map (fun x -> downloadOne (Helper.Crypto(x,Helper.M15)) startTime endTime)
                    |> Async.Parallel
                    |> Async.RunSynchronously
                    |> ignore*)
        


    
    

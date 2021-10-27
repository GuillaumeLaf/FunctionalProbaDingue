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

    let defaultState interval = State(Crypto("DefaultCrypto"), interval)

    let intervalToString = function
        | M15 -> "M15"

    let intervalToBinanceInterval = function
        | M15 -> Enums.KlineInterval.FifteenMinutes

    let getPath = function
        | Download -> pathDL
        | Aggregate -> path

    let cryptoNameM = Monad.M (fun (State(Crypto(name),interval)) -> name,State(Crypto(name),interval))
    let cryptoIntervalM = Monad.M (fun (State(Crypto(name),interval)) -> interval,State(Crypto(name),interval))
    let updateSymbolM newSymbol = Monad.M (fun (State(Crypto(_),interval)) -> (),State(Crypto(newSymbol),interval))

    let getCryptoPath t name interval = Path.Combine(getPath t, intervalToString interval, name + ".csv")
    let cryptoPathM t = getCryptoPath t <!> cryptoNameM <*> cryptoIntervalM
    let cryptoExistsM t = File.Exists <!> cryptoPathM t

    let getIntervalpath t interval = Path.Combine(getPath t, intervalToString interval)
    let intervalPathM t = getIntervalpath t <!> cryptoIntervalM
    let intervalExistsM t = Directory.Exists <!> intervalPathM t

    let symbolsList = File.ReadAllLines(Path.Combine(pathDL, "Symbols.csv"))

    let isHeader (row:string) = row.[0..4] = "Close"

    let fileDefaultValue t defaultValue fileM = 
        Monad.state{
            let! doesExists = cryptoExistsM t
            if doesExists then 
                return! fileM
            else return defaultValue
        }

    let cryptoDataM t = File.ReadAllLines <!> cryptoPathM t

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

    let downloadAll interval startTime endTime = 
        let result = Helper.symbolsList |> Array.map (fun s -> Monad.state{do! Helper.updateSymbolM s
                                                                           return! downloadOneM startTime endTime})
                                        |> Monad.mapM   
                                        >>= (fun xs -> (xs,5) |> Async.Parallel |> Async.RunSynchronously |> Monad.rets)
        Monad.run result (Helper.defaultState interval) |> ignore

(*    let download cryptoNames startTime endTime = 
        cryptoNames |> Array.map (fun x -> downloadOne (Helper.Crypto(x,Helper.M15)) startTime endTime)
                    |> Async.Parallel
                    |> Async.RunSynchronously
                    |> ignore*)
        
module Aggregator =   
    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x

    let lastTimeM () = Helper.lastTimeM Helper.Aggregate
    let intevalExistsM () = Helper.intervalExistsM Helper.Aggregate
    let cryptoExistsM () = Helper.cryptoExistsM Helper.Aggregate

    let compareOpenTimes (dbTime:DateTime) dlTime = if (dlTime - dbTime).TotalDays > 0.0 then true else false
        
    let isRowValidForAggregation dbTime (rowString:string) = 
        rowString |> Helper.extractOpenTime
                  |> Helper.parseDate
                  |> compareOpenTimes dbTime

    let checkHeaderM sq = 
        Monad.state{
        let! doesExists = cryptoExistsM ()
        match doesExists with
            | true -> return sq
            | false -> return seq {yield Helper.header; yield! sq}
        }

    let aggregateOne data lastDbTime crypto interval = 
        async{
            printfn "%s" ("aggregating " + crypto)
            data
                |> Seq.skip 1
                |> Seq.filter(fun x -> x |> isRowValidForAggregation lastDbTime)
                |> (fun xs -> Monad.run (checkHeaderM xs) (Helper.State(Helper.Crypto(crypto),interval)) |> fst)
                |> (fun newLines -> File.AppendAllLines(Helper.getCryptoPath Helper.Aggregate crypto interval, newLines))
        }

    let aggregateOneM () = aggregateOne <!> Helper.cryptoDataM Helper.Download <*> Helper.lastTimeM Helper.Aggregate
                                        <*> Helper.cryptoNameM <*> Helper.cryptoIntervalM

    let aggregateAll interval = 
        let result = Helper.symbolsList |> Array.map (fun s -> Monad.state{do! Helper.updateSymbolM s
                                                                           return! aggregateOneM()})
                                        |> Monad.mapM   
                                        >>= (fun xs -> (xs,5) |> Async.Parallel |> Async.RunSynchronously |> Monad.rets)
        Monad.run result (Helper.defaultState interval) |> ignore

(*    let aggregateAll () = 
        Helper.getSymbols() |> Seq.map (fun x -> aggregateOne (Helper.Crypto(x,Helper.M15)))
                            |> (fun x -> Async.Parallel (x,5))
                            |> Async.RunSynchronously
                            |> ignore*)
    
        

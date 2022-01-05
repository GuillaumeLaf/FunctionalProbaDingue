namespace DataBase

open System
open System.Data
open FSharp.Data.Sql
open System.IO
open System.Linq

open Binance

// MS Access has strong limitations (max. 16M rows per table and max. 2G file) -> go with SQL Server (lighter limitations)
module DataBase = 
    let path = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\Binance"
    let pathDL = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\NewData"

    type sqlReal = SqlDataProvider<Common.DatabaseProviderTypes.MSSQLSERVER, "Server=localhost;Database=BinanceDB;User Id=sa;Password=123">

    let ctxReal = sqlReal.GetDataContext()

    module Table = 
        
        let tTimeSeriesCtx = ctxReal.Dbo.TTimeSeries
        let tTickersCtx = ctxReal.Dbo.TTickers

        type Table = 
            | TableTimeSeries
            | TableTickers

        type TableKey = 
            | TimeSeriesKey of string * DateTime
            | TickersKey of string

        type TableData = 
            | TimeSeriesData of string * DateTime * float * float * float * float * float * float * int * DateTime
            | TickersData of string

        let createRow = function
            | TimeSeriesData(name,closeT,openP,highP,lowP,closeP,quote,baseV,trade,openT) -> let r = tTimeSeriesCtx.Create()
                                                                                             r.Ticker <- name
                                                                                             r.CloseTime <- closeT
                                                                                             r.OpenPrice <- openP
                                                                                             r.HighPrice <- highP
                                                                                             r.LowPrice <- lowP
                                                                                             r.ClosePrice <- closeP
                                                                                             r.QuoteVolume <- quote
                                                                                             r.BaseVolume <- baseV
                                                                                             r.TradeCount <- trade
                                                                                             r.OpenTime <- openT
                                                                                             ctxReal.SubmitUpdates() 
            | TickersData(name) -> let r = tTickersCtx.Create()
                                   r.Ticker <- name
                                   ctxReal.SubmitUpdates()
                                                         
        let importFromAggregate crypto = 
            let (Helper.Crypto(name,_)) = crypto
            use stream = new StreamReader(Helper.cryptoPath crypto Helper.Aggregate)
            stream.ReadLine() |> ignore // skip the header row
            let mutable line = ""

            while (line <- stream.ReadLine(); line <> null) do
                let splitted = line.Split ';'
                let data = TimeSeriesData(name,splitted.[0] |> DataPreps.formatTime,
                                          splitted.[1] |> float,
                                          splitted.[2] |> float,
                                          splitted.[3] |> float,
                                          splitted.[4] |> float,
                                          splitted.[5] |> float,
                                          splitted.[6] |> float,
                                          splitted.[7] |> int,
                                          splitted.[8] |> DataPreps.formatTime)
                createRow data


        let queryTimeSeriesRows ticker from_ to_ = 
            query{for p in tTimeSeriesCtx do 
                  where (p.Ticker=ticker && p.OpenTime>=from_ && p.OpenTime<=to_)
                  select p}

        // https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/query-expressions

        let queryTickersRows (tickers:seq<string>) = 
            let innerQuery = 
                query {
                    for t in tickers do
                    select t
                }
            query {
                  for p in tTickersCtx do 
                  where (innerQuery.Contains(p.Ticker))
                  select p
            }

        let getTimeSeriesRows ticker from_ to_ = 
             queryTimeSeriesRows ticker from_ to_
             |> Seq.map(fun r -> TimeSeriesData(r.Ticker,r.CloseTime,r.OpenPrice,r.HighPrice,r.LowPrice,r.ClosePrice,r.QuoteVolume,r.BaseVolume,r.TradeCount,r.OpenTime))

        let getElement = function
            | TimeSeriesKey(name,openT) -> getTimeSeriesRows name openT openT
            | TickersKey(name) -> queryTickersRows (seq{yield name})
                                  |> Seq.map(fun r -> TickersData(r.Ticker))

        let updateElement = function
            | TimeSeriesData(name,closeT,openP,highP,lowP,closeP,quote,baseV,trade,openT) -> queryTimeSeriesRows name openT openT
                                                                                                |> Seq.iter (fun r -> r.CloseTime <- closeT
                                                                                                                      r.OpenPrice <- openP
                                                                                                                      r.HighPrice <- highP
                                                                                                                      r.LowPrice <- lowP
                                                                                                                      r.ClosePrice <- closeP
                                                                                                                      r.QuoteVolume <- quote
                                                                                                                      r.BaseVolume <- baseV
                                                                                                                      r.TradeCount <- trade)          
                                                                                             ctxReal.SubmitUpdates()
            | TickersData(name) -> queryTickersRows (seq{yield name})
                                    |> Seq.iter (fun r -> r.Ticker <- name)
                                   ctxReal.SubmitUpdates()


            



    
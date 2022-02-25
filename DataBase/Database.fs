namespace Database

open System
open System.Data
open FSharp.Data.Sql
open System.IO
open System.Linq

module DB = 
    let path = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\Binance"

    type sqlReal = SqlDataProvider<Common.DatabaseProviderTypes.MSSQLSERVER, "Server=localhost;Database=BinanceDB;User Id=sa;Password=123">

    let ctxReal = sqlReal.GetDataContext()

    [<RequireQualifiedAccess>]
    module Table = 
        
        [<RequireQualifiedAccess>]
        module Timeseries = 
            
            [<Struct>]
            type Key = Key of string * DateTime

            type Data = 
                { Ticker:string;
                  CloseTime:DateTime;
                  OpenPrice:float;
                  HighPrice:float;
                  LowPrice:float;
                  ClosePrice:float;
                  QuoteVolume:float;
                  BaseVolume:float;
                  TradeCount:int;
                  OpenTime:DateTime }

            let context = ctxReal.Dbo.TTimeSeries

            let createRow (data:Data) = 
                let t = context.Create()
                t.Ticker <- data.Ticker
                t.CloseTime <- data.CloseTime
                t.OpenPrice <- data.OpenPrice
                t.HighPrice <- data.HighPrice
                t.LowPrice <- data.LowPrice
                t.ClosePrice <- data.ClosePrice
                t.QuoteVolume <- data.QuoteVolume
                t.BaseVolume <- data.BaseVolume
                t.TradeCount <- data.TradeCount
                t.OpenTime <- data.OpenTime
                ctxReal.SubmitUpdates()

            [<RequireQualifiedAccess>]
            module Query = 

                let get ticker from_ to_ = 
                    query {
                        for p in context do 
                        where (p.Ticker=ticker && p.OpenTime>=from_ && p.OpenTime<to_)
                        select p
                    } 

                let closePrice ticker from_ to_ = 
                    query {
                        for p in context do
                        where (p.Ticker=ticker && p.OpenTime>=from_ && p.OpenTime<to_)
                        select (p.CloseTime, [|p.ClosePrice|])
                    }
                    
                // Time is given by the first ticker ! should change to get the maximum time available for the given tickers. 
                let closePrices (tickers:string[]) from_ to_ =
                    let mutable tmp = closePrice tickers.[0] from_ to_
                    for t in Array.tail tickers do 
                        tmp <- query {
                                for (t1,arr) in tmp do
                                join (t2,p2) in closePrice t from_ to_ on (t1=t2)
                                select (t1,Array.append arr p2)
                               }
                    tmp
                        
                

            let closePrices tickers from_ to_ = Query.closePrices tickers from_ to_ |> Seq.toArray

            let multipleGet ticker1 ticker2 from_ to_ = 
                query {
                    for (t1,p1) in Query.closePrice ticker1 from_ to_ do
                    join (t2,p2) in Query.closePrice ticker2 from_ to_ on (t1 = t2)
                    select (t1,[|p1;p2|])
                } |> Seq.toArray

            let get ticker from_ to_ = Query.get ticker from_ to_ |>Seq.map (fun x -> x.MapTo<Data>())
                
            let getClosePrice ticker from_ to_ = Query.closePrice ticker from_ to_ |> Seq.toArray

        [<RequireQualifiedAccess>]
        module Tickers = 

            type Key = Key of string
            
            let tTickersCtx = ctxReal.Dbo.TTickers







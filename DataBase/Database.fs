namespace Database

open System
open System.Data
open FSharp.Data.Sql
open System.IO
open System.Linq

module DB = 

    [<Literal>]
    let path = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\Binance"

    [<Literal>]
    let connectionString = "Server=localhost;Database=BinanceDB;User Id=sa;Password=123"

    type sqlReal = SqlDataProvider<Common.DatabaseProviderTypes.MSSQLSERVER, 
                                   connectionString,
                                   UseOptionTypes=true>

    let ctxReal = sqlReal.GetDataContext()

    [<RequireQualifiedAccess>]
    module Table = 
        
        [<RequireQualifiedAccess>]
        module Timeseries = 
            
            [<Struct>]
            type Key = Key of ticker:string * openTime:DateTime

            type Data = 
                { Ticker:string;                // Key
                  CloseTime:DateTime option;
                  OpenPrice:float option;
                  HighPrice:float option;
                  LowPrice:float option;
                  ClosePrice:float option;
                  QuoteVolume:float option;
                  BaseVolume:float option;
                  TradeCount:int option;
                  OpenTime:DateTime }           // Key

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
                        where (p.Ticker=ticker && 
                               p.OpenTime >= Option.defaultValue DateTime.MinValue.Date from_ && 
                               p.OpenTime < Option.defaultValue DateTime.MaxValue.Date to_)
                        select p
                    } 

                let closeTime ticker from_ to_ = 
                    query {
                        for p in context do
                        where (p.Ticker=ticker && 
                               p.OpenTime >= Option.defaultValue DateTime.MinValue.Date from_ && 
                               p.OpenTime < Option.defaultValue DateTime.MaxValue.Date to_)
                        select p.CloseTime
                    }

                // Get the union of the closetime for all given tickers.
                // The final query is not sorted !
                let datetimeUnions (tickers:string[]) from_ to_ =
                    let foldUnion = Array.tail >> Array.fold (fun (s:IQueryable<DateTime option>) x -> (closeTime x from_ to_).Union(s)) (closeTime tickers.[0] from_ to_)
                    query {
                        for t in foldUnion tickers do 
                        select t
                    }

                let closePrice ticker from_ to_ = 
                    query {
                        for p in context do
                        where (p.Ticker=ticker && 
                               p.OpenTime >= Option.defaultValue DateTime.MinValue.Date from_ && 
                               p.OpenTime < Option.defaultValue DateTime.MaxValue.Date to_)
                        select (p.CloseTime, p.ClosePrice)
                    }

            let closePrices (tickers:string[]) from_ to_ = 
                let dts = Query.datetimeUnions tickers from_ to_ |> Seq.sort 
                                                                 |> Seq.map (fun x -> String.concat " " [|x.Value.ToString("yyyy/MM/dd HH:mm:ss")|]) 
                                                                 |> Seq.toArray

                Utils.createTemporaryDatetimeTable
                Utils.populateColumn "tmpDatetime" dts

                Utils.createTemporaryTableFor tickers.[0] from_ to_
                Utils.joinTemporaryTablesOnce tickers.[0]

                for ticker in Array.tail tickers do 
                    Utils.createTemporaryTableFor ticker from_ to_
                    Utils.joinTemporaryTables ticker


                        
            // let closePrices tickers from_ to_ = Query.closePrices tickers from_ to_ |> Seq.toArray

            let get ticker from_ to_ = Query.get ticker from_ to_ |>Seq.map (fun x -> x.MapTo<Data>())
                
            let getClosePrice ticker from_ to_ = Query.closePrice ticker from_ to_ |> Seq.toArray

            let datetimeUnions (tickers:string[]) from_ to_ = Query.datetimeUnions tickers from_ to_ |> Seq.sort |> Seq.toArray

(*            let closePrices (tickers:string[]) from_ to_ = 
                let time = datetimeUnions tickers from_ to_*)


        [<RequireQualifiedAccess>]
        module Tickers = 

            type Key = Key of string
            
            let tTickersCtx = ctxReal.Dbo.TTickers







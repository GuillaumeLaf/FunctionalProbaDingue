namespace DataBase

open System
open System.Data
open FSharp.Data.Sql
open System.IO

open Binance

// MS Access has strong limitations (max. 16M rows per table and max. 2G file) -> go with SQL Server (lighter limitations)
module DataBaseTest = 
    let path = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\Binance"
    let pathDL = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\NewData"

    type sqlTest = SqlDataProvider<Common.DatabaseProviderTypes.MSSQLSERVER, "Server=localhost;Database=TestBinanceDB;User Id=sa;Password=123">

    let ctxTest = sqlTest.GetDataContext()

    module Table = 
        
        let tTimeSeriesCtx = ctxTest.Dbo.TTimeSeries
        let tTickersCtx = ctxTest.Dbo.TTickers

        type Table = 
            | TableTimeSeries
            | TableTickers

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
                                                                                             ctxTest.SubmitUpdates()
            | TickersData(name) -> let r = tTickersCtx.Create()
                                   r.Ticker <- name
                                   ctxTest.SubmitUpdates()

        let importFromAggregate crypto = 
            let (Helper.Crypto(name,_)) = crypto
            use stream = new StreamReader(Helper.cryptoPath crypto Helper.Aggregate)
            stream.ReadLine() |> ignore // skip the header row
            let mutable line = ""
            let mutable count = 0

            while (line <- stream.ReadLine(); count <- count+1; line <> null && count < 100) do
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



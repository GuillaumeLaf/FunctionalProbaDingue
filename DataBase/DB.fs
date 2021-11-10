﻿namespace DataBase

open System
open System.Data
open FSharp.Data.Sql
open System.IO

open Binance

// MS Access has strong limitations (max. 16M rows per table and max. 2G file) -> go with SQL Server (lighter limitations)

module DB = 
    let path = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\Binance"
    let pathDL = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\NewData"

    type sqlTest = SqlDataProvider<Common.DatabaseProviderTypes.MSSQLSERVER, "Server=localhost;Database=TestBinanceDB;User Id=sa;Password=123">
    type sqlReal = SqlDataProvider<Common.DatabaseProviderTypes.MSSQLSERVER, "Server=localhost;Database=BinanceDB;User Id=sa;Password=123">

    type Provider = 
        | TestProvider of sqlTest.dataContext
        | RealProvider of sqlReal.dataContext

    type DBType = 
        | Test
        | Real 

    type Table = 
        | TableTimeSeries
        | TableTickers

    type TableContextTest = 
        | TimeSeriesContextTest of sqlTest.dataContext.dboSchema.``dbo.tTimeSeries``
        | TickersContextTest of sqlTest.dataContext.dboSchema.``dbo.tTickers``

    type TableContextReal = 
        | TimeSeriesContextReal of sqlReal.dataContext.dboSchema.``dbo.tTimeSeries``
        | TickersContextReal of sqlReal.dataContext.dboSchema.``dbo.tTickers``

    type TableContext = 
        | TestContext of TableContextTest
        | RealContext of TableContextReal

    let dbContext = function
        | Test -> sqlTest.GetDataContext() |> TestProvider
        | Real -> sqlReal.GetDataContext() |> RealProvider

    let getTestContext = (function TestProvider(x) -> x | RealProvider(_) -> invalidArg "Context" "Try accessing RealContext with TestContextFunction. Try using 'getRealContext'.'") 
    let getRealContext = (function RealProvider(x) -> x | TestProvider(_) -> invalidArg "Context" "Try accessing TestContext with RealContextFunction. Try using 'getTestContext'.'") 

    let applyContextTo fTest fReal = function
        | Test as x -> (dbContext >> getTestContext >> fTest) x
        | Real as x -> (dbContext >> getRealContext >> fReal) x

    let getTableContext = function
        | TableTimeSeries -> applyContextTo (fun c -> (TimeSeriesContextTest >> TestContext) c.Dbo.TTimeSeries)
                                            (fun c-> (TimeSeriesContextReal >> RealContext) c.Dbo.TTimeSeries)
        | TableTickers -> applyContextTo (fun c -> (TickersContextTest >> TestContext) c.Dbo.TTickers)
                                         (fun c -> (TickersContextReal >> RealContext) c.Dbo.TTickers)

    let ctx = sqlReal.GetDataContext()
    let ts = ctx.Dbo.TTimeSeries

    let getTableName = function
        | TableTimeSeries -> "tTimeSeries"
        | TableTickers -> "tTickers"

    let formatTime (timeString:string) = 
        let splittedString = timeString.Split ' '
        let dateComps = splittedString.[0].Split '/'
        let timeComps = splittedString.[1].Split ':'
        new DateTime(int dateComps.[2],int dateComps.[0],int dateComps.[1],int timeComps.[0],int timeComps.[1],int timeComps.[2])

    let importFromAggregate crypto dbType = 
        let (Helper.Crypto(name,_)) = crypto
        printfn "%s" ("Importing " + name)
        use stream = new StreamReader(Helper.cryptoPath crypto Helper.Aggregate)
        stream.ReadLine() |> ignore // skip the header row
        let mutable line = ""
        let mutable count = 0

        let cond = function
            | Test -> line <- stream.ReadLine() 
                      count<-count+1
                      line <> null && count < 100
            | Real -> line <- stream.ReadLine()
                      line <> null

        while (cond dbType) do
            let splitted = line.Split ';'
            let row = ts.Create()
            row.Ticker <- name
            row.CloseTime <- splitted.[0] |> formatTime
            row.OpenPrice <- splitted.[1] |> float
            row.HighPrice <- splitted.[2] |> float
            row.LowPrice <- splitted.[3] |> float
            row.ClosePrice <- splitted.[4] |> float
            row.QuoteVolume <- splitted.[5] |> float
            row.BaseVolume <- splitted.[6] |> float
            row.TradeCount <- splitted.[7] |> int
            row.OpenTime <- splitted.[8] |> formatTime
            ctx.SubmitUpdates()
        


    // https://stackoverflow.com/questions/31070731/f-sharp-saving-record-type-into-access-db

(*   

    let builder = new OleDbConnectionStringBuilder()
    builder.ConnectionString <- @"Data Source=C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\[TEST]BinanceDB.accdb"
    builder.Add("Provider", "Microsoft.ACE.OLEDB.12.0")

*)

(*    let getData = 
        let conn = new OleDbConnection(builder.ConnectionString)
        conn.Open()
        let cmd = new OleDbCommand()
        cmd.CommandType <- CommandType.Text
        //let selectCmd = "SELECT name_, age_ FROM table1"
        let selectCmd = "INSERT INTO table1 (name_, age_) VALUES ('marie', 3)";
        cmd.CommandText <- selectCmd
        cmd.Connection <- conn
        cmd.ExecuteNonQuery() |> ignore 
*)(*        let reader = cmd.ExecuteReader()
        while (reader.Read()) do
            printfn "%A" (reader.GetInt32(1))
        reader.Close()*)(*
        conn.Close()*)
//https://social.msdn.microsoft.com/Forums/sqlserver/en-US/c6410e34-52ba-4c30-bac4-535101d172f3/f-how-to-improve-performance-for-inserting-records-into-sql-server-database?forum=fsharpgeneral



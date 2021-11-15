namespace DataBase

open System
open System.Data
open FSharp.Data.Sql
open System.IO

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





            



    (*

    module Table2 =
        type Table = 
            | TableTimeSeries
            | TableTickers

        type TableData = 
            | TimeSeriesData of string * DateTime * float * float * float * float * float * float * int * DateTime
            | TickersData of string

        type TableDataFunc = TableDataFunc of (TableData -> unit)

        type TableContextTest = 
            | TimeSeriesContextTest of sqlTest.dataContext.dboSchema.``dbo.tTimeSeries``
            | TickersContextTest of sqlTest.dataContext.dboSchema.``dbo.tTickers``

        type TableContextReal = 
            | TimeSeriesContextReal of sqlReal.dataContext.dboSchema.``dbo.tTimeSeries``
            | TickersContextReal of sqlReal.dataContext.dboSchema.``dbo.tTickers``

        type TableContext = 
            | TestContext of TableContextTest
            | RealContext of TableContextReal

        let getTableTestContext = (function TestContext(c) -> c | RealContext(_) -> invalidArg "Context" "Try accessing RealContext with TestContextFunction. Try using 'getRealContext'.'")
        let getTableRealContext = (function RealContext(c) -> c | TestContext(_) -> invalidArg "Context" "Try accessing TestContext with RealContextFunction. Try using 'getTestContext'.'")

        let getTableTimeSeriesTestContext = (function TimeSeriesContextTest(c) -> c | TickersContextTest(_) -> invalidArg "Context" "Try accessing TimeSeriesContextTest with RealContextFunction. Try using 'getTableTimeSeriesRealContext'.'")
        let getTableTimeSeriesRealContext = (function TimeSeriesContextReal(c) -> c | TickersContextReal(_) -> invalidArg "Context" "Try accessing TimeSeriesContextReal with TestContextFunction. Try using 'getTableTimeSeriesTestContext'.'")
        let getTableTickersTestContext = (function TickersContextTest(c) -> c | TimeSeriesContextTest(_) -> invalidArg "Context" "Try accessing TickersContextTest with RealContextFunction. Try using 'getTableTickersRealContext'.'")
        let getTableTickersRealContext = (function TickersContextReal(c) -> c | TimeSeriesContextReal(_) -> invalidArg "Context" "Try accessing TickersContextReal with TestContextFunction. Try using 'getTableTickersTestContext'.'")

        let getTableContext = function
            | TableTimeSeries -> DB.applyDbContextTo (fun c -> (TimeSeriesContextTest >> TestContext) c.Dbo.TTimeSeries)
                                                     (fun c-> (TimeSeriesContextReal >> RealContext) c.Dbo.TTimeSeries)
            | TableTickers -> DB.applyDbContextTo (fun c -> (TickersContextTest >> TestContext) c.Dbo.TTickers)
                                                  (fun c -> (TickersContextReal >> RealContext) c.Dbo.TTickers)

        let applyToTimeSeriesTable fTest fReal = function
            | DB.Test as t -> (getTableContext TableTimeSeries >> getTableTestContext >> getTableTimeSeriesTestContext >> fTest) t 
            | DB.Real as t -> (getTableContext TableTimeSeries >> getTableRealContext >> getTableTimeSeriesRealContext >> fReal) t

        let applyToTickersTable fTest fReal = function
            | DB.Test as t -> (getTableContext TableTickers >> getTableTestContext >> getTableTickersTestContext >> fTest) t 
            | DB.Real as t -> (getTableContext TableTickers >> getTableRealContext >> getTableTickersRealContext >> fReal) t

        let createTimeSeriesRow = function
            | 

        let createRow dbType = function
            | TimeSeriesData(name,closeT,openP,highP,lowP,closeP,quote,baseV,trade,openT) -> let newRow = applyToTimeSeriesTable (fun c -> c.Create()) (fun c -> c.Create()) dbType
                                                                                             newRow.Ticker <- name
                                                                                             newRow.CloseTime <- closeT
                                                                                             newRow.OpenPrice <- openP
                                                                                             newRow.HighPrice <- highP
                                                                                             newRow.LowPrice <- lowP
                                                                                             newRow.ClosePrice <- closeP
                                                                                             newRow.QuoteVolume <- quote
                                                                                             newRow.BaseVolume <- baseV
                                                                                             newRow.TradeCount <- trade
                                                                                             newRow.OpenTime <- openT

        let formatTime (timeString:string) = 
            let splittedString = timeString.Split ' '
            let dateComps = splittedString.[0].Split '/'
            let timeComps = splittedString.[1].Split ':'
            new DateTime(int dateComps.[2],int dateComps.[0],int dateComps.[1],int timeComps.[0],int timeComps.[1],int timeComps.[2])
*)
(*        let importFromAggregate crypto dbType = 
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
                ctx.SubmitUpdates()*)
        


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



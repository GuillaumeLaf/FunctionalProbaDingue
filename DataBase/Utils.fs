namespace Database

open System
open System.Data
open FSharp.Data.Sql
open FSharp.Data
open System.Data.SqlClient
open System.IO
open System.Linq

module Utils = 

    // https://stackoverflow.com/questions/31702614/creating-table-in-sql-server-using-f

    [<Literal>]
    let path = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\Binance"

    [<Literal>]
    let connectionString = "Server=localhost;Database=BinanceDB;User Id=sa;Password=123"

    let execute sqlString = 
        let conn = new SqlConnection(connectionString)
        conn.Open()
        let cmd = new SqlCommand(sqlString,conn)
        cmd.CommandType <- CommandType.Text
        cmd.ExecuteNonQuery() |> ignore
        conn.Close()

    let executeList sqlStringList = 
        let conn = new SqlConnection(connectionString)
        conn.Open()
        let exec c = 
            let cmd = new SqlCommand(c,conn)
            //printfn "%A" sqlStringList
            cmd.CommandType <- CommandType.Text
            cmd.ExecuteNonQuery() |> ignore
        List.map exec sqlStringList |> ignore
        conn.Close()

    let createTemporaryDatetimeTable = 
        new String(
            seq { 
                yield sprintf "CREATE TABLE tmpDatetime (\n"
                yield sprintf "[dt] datetime PRIMARY KEY,\n" 
                yield ")"
                } |> Seq.concat |> Seq.toArray) |> execute

    let deleteTable tableName = "DROP TABLE " + tableName |> execute
    
    // Table must be created beforehand
    let populateColumn tableName (data:string[]) = 
        data |> Seq.chunkBySize 1000 
             |> Seq.map (Seq.map (fun x -> "('" + x + "')") 
                            >> Seq.fold (fun s x -> s + x + ", ") "" 
                            >> (+) ("INSERT INTO " + tableName + " VALUES ")
                            >> (fun (x:string) -> x.[..x.Length-3] + ";" ))
             |> Seq.toList
             |> executeList


    let createTemporaryTableFor ticker from_ to_ = 
        let fromV = (Option.defaultValue DateTime.MinValue.Date from_).ToString("yyyy/MM/dd HH:mm:ss")
        let toV = (Option.defaultValue DateTime.MaxValue.Date to_).ToString("yyyy/MM/dd HH:mm:ss")
        "SELECT CloseTime, ClosePrice INTO tmp" + ticker + "\n FROM tTimeSeries \n WHERE Ticker='" + ticker + 
        "'\n AND OpenTime >= '" + fromV + "'\n AND OpenTime < '" + toV + "';"
        |> execute

    let joinTemporaryTablesOnce ticker =
        ["SELECT * \n INTO tmpJoin \n FROM tmpDateTime \n LEFT JOIN tmp" + ticker + " ON tmpDatetime.dt=tmp" + ticker + ".CloseTime;";
         "ALTER TABLE tmpJoin \n DROP COLUMN CloseTime;";
         "EXECUTE sp_rename N'dbo.tmpJoin.ClosePrice', N'" + ticker + "', 'COLUMN';";
         "DROP TABLE tmp" + ticker + ";"] |> executeList

    let joinTemporaryTables ticker =
        ["SELECT * \n INTO tmpJoin" + ticker + " \n FROM tmpJoin \n LEFT JOIN tmp" + ticker + " ON tmpJoin.dt=tmp" + ticker + ".CloseTime;";
         "DROP TABLE tmpJoin;";
         "EXECUTE sp_rename N'dbo.tmpJoin" + ticker + "', N'tmpJoin';";
         "ALTER TABLE tmpJoin \n DROP COLUMN CloseTime;";
         "EXECUTE sp_rename N'dbo.tmpJoin.ClosePrice', N'" + ticker + "', 'COLUMN';";
         "DROP TABLE tmp" + ticker + ";"] |> executeList

    

        


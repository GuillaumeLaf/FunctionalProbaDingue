namespace Database

open System
open System.Data
open FSharp.Data.Sql
open FSharp.Data
open System.Data.SqlClient
open System.IO
open System.Linq

module Utils = 
    // F# computation expression for SQL has its limitations.
    // This module uses SQL command to manage the DB. 

    // https://stackoverflow.com/questions/31702614/creating-table-in-sql-server-using-f

    [<Literal>]
    let path = "C:\Users\Guillaume\OneDrive\Trading\FSharp\Data\Binance"

    [<Literal>]
    let connectionString = "Server=localhost;Database=BinanceDB;User Id=sa;Password=123"

    // Execute the given SQL command stored as string.
    // The SQL command cannot be a query.
    let execute sqlString = 
        let conn = new SqlConnection(connectionString)
        conn.Open()
        let cmd = new SqlCommand(sqlString,conn)
        cmd.CommandType <- CommandType.Text
        cmd.ExecuteNonQuery() |> ignore
        conn.Close()

    // Same as execute command but works with 'List'.
    // It will only open and close the connection with the DB once (not between each element of the list). 
    let executeList sqlStringList = 
        let conn = new SqlConnection(connectionString)
        conn.Open()
        let exec c = 
            let cmd = new SqlCommand(c,conn)
            cmd.CommandType <- CommandType.Text
            cmd.ExecuteNonQuery() |> ignore
        List.map exec sqlStringList |> ignore
        conn.Close()

    // Create an empty table that will receive the 'Datetime' for the tickers
    let createTemporaryDatetimeTable = 
        new String(
            seq { 
                yield sprintf "CREATE TABLE tmpDatetime (\n"
                yield sprintf "[dt] datetime PRIMARY KEY,\n" 
                yield ")"
                } |> Seq.concat |> Seq.toArray) |> execute

    // General function deleting a given table
    let deleteTable tableName = "DROP TABLE " + tableName |> execute
    
    // Insert a column of data to the given table.
    // Insert by batch of 1000 (max. allowed by the 'INSERT' sql command).
    // Table 'tableName' must be created beforehand
    let populateColumn tableName (data:string[]) = 
        data |> Seq.chunkBySize 1000 
             |> Seq.map (Seq.map (fun x -> "('" + x + "')") 
                            >> Seq.fold (fun s x -> s + x + ", ") "" 
                            >> (+) ("INSERT INTO " + tableName + " VALUES ")
                            >> (fun (x:string) -> x.[..x.Length-3] + ";" ))
             |> Seq.toList
             |> executeList

    // Create temporary table for the given ticker filled with 'CloseTime' and 'ClosePrice' between the two given dates.
    let createTemporaryTableFor ticker from_ to_ = 
        let fromV = (Option.defaultValue DateTime.MinValue.Date from_).ToString("yyyy/MM/dd HH:mm:ss")
        let toV = (Option.defaultValue DateTime.MaxValue.Date to_).ToString("yyyy/MM/dd HH:mm:ss")
        "SELECT CloseTime, ClosePrice INTO tmp" + ticker + "\n FROM tTimeSeries \n WHERE Ticker='" + ticker + 
        "'\n AND OpenTime >= '" + fromV + "'\n AND OpenTime < '" + toV + "';"
        |> execute

    // Join the data from the temporary ticker table to the aggregate table 
    // This function is need to go from the 'tmpDatetime' (containing all the datetime of every ticker)
    // to the final aggregate table.
    let joinTemporaryTablesOnce ticker =
        ["SELECT * \n INTO tmpJoin \n FROM tmpDateTime \n LEFT JOIN tmp" + ticker + " ON tmpDatetime.dt=tmp" + ticker + ".CloseTime;";
         "ALTER TABLE tmpJoin \n DROP COLUMN CloseTime;";
         "EXECUTE sp_rename N'dbo.tmpJoin.ClosePrice', N'" + ticker + "', 'COLUMN';";
         "DROP TABLE tmp" + ticker + ";"] |> executeList

    // Join the data from the temporary ticker table to the aggregate table 
    let joinTemporaryTables ticker =
        ["SELECT * \n INTO tmpJoin" + ticker + " \n FROM tmpJoin \n LEFT JOIN tmp" + ticker + " ON tmpJoin.dt=tmp" + ticker + ".CloseTime;";
         "DROP TABLE tmpJoin;";
         "EXECUTE sp_rename N'dbo.tmpJoin" + ticker + "', N'tmpJoin';";
         "ALTER TABLE tmpJoin \n DROP COLUMN CloseTime;";
         "EXECUTE sp_rename N'dbo.tmpJoin.ClosePrice', N'" + ticker + "', 'COLUMN';";
         "DROP TABLE tmp" + ticker + ";"] |> executeList

    // Retrieve the data contained in the temporary join table.
    // 'tmpJoin' table must already be initialized.
    let selectFromTmpJoinTable (tickers:string[]) = 
        seq {
            use conn = new SqlConnection(connectionString)
            use cmd = new SqlCommand("SELECT * FROM tmpJoin", conn)
            cmd.CommandType <- CommandType.Text
            conn.Open()
            use reader = cmd.ExecuteReader()
            while reader.Read() do
                yield [| for ticker in tickers do if reader.[ticker] = DBNull.Value then None else Some reader.[ticker] |] 
        }
        |> Seq.toArray


        
            
            
    

        


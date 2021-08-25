module CSVManagement

open FSharp.Data

let btc = CsvFile.Load("BTCUSDT.csv").Rows
                |> Seq.map (fun row -> row.Columns.[4])
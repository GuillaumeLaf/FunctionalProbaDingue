namespace Timeseries

open FSharp.Charting
open TimeseriesType

module Plot = 
    
    let allSeries (ts:TimeseriesType.TS) = Chart.Rows([for i in 0..ts.Size-1 do (TS.dataDefault >> Array2D.row i >> Chart.Line) ts]) |> Chart.Show

    let onlySeries (idx:int[]) (ts:TimeseriesType.TS) = Chart.Rows([for i in idx do (TS.dataDefault >> Array2D.row i >> Chart.Line) ts]) |> Chart.Show
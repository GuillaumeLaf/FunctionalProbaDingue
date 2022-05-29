namespace Timeseries

open FSharp.Charting
open TimeseriesType

module Plot = 
    let x = 0
(*    let allSeries (ts:TimeseriesType.TS<'T>) = Chart.Rows([for i in 0..ts.Size-1 do (TS.dataDefault >> Array2D.row i >> Chart.Line) ts]) |> Chart.Show

    let onlySeries (idx:int[]) (ts:TimeseriesType.TS<float32 option>) = Chart.Rows([for i in idx do (TS.dataDefault >> Array2D.row i >> Chart.Line) ts]) |> Chart.Show*)
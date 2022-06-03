namespace Timeseries

module Transformations = 
    open Xunit
    open Timeseries
    open Timeseries.TimeseriesType
    open Timeseries.TimeseriesState
    open FSharpPlus.Data
    open FSharpPlus

    let ts = Array2D.init 2 3 (fun i j -> (float32 i)*3f + float32 j) |> TS.create

    let transfo:obj[] list = 
        [
            [| [ Center(None) ]; array2D >> TS.create >> TS.addTransformation (Center(Some [|1f;4f|])) <| [|[|-1f;0f;1f|]; [|-1f;0f;1f|]|] |]
        ]
    [<Theory>]
    [<MemberData(nameof(transfo))>]
    let ``Center`` trsfo expected = Assert.Equal<TS<float32>>(expected, Transformations.forward trsfo ts) 









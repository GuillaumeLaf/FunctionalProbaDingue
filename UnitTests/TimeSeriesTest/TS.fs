namespace TimeseriesTest

module TS =
    open Xunit
    open Timeseries
    open Timeseries.TimeseriesType
    open FSharpPlus.Data
    open FSharpPlus

    let ts = Array2D.init 2 3 (fun i j -> i*3 + j) |> TS.create

    [<Fact>]
    let ``dataDefault`` () = Assert.Equal<int[,]>(array2D [|[|0;0;0|];[|0;0;0|]|], TS.dataDefault ts) 
        
    [<Fact>]
    let ``pctLength`` () = Assert.Equal<int>(1, TS.pctLength 50f ts)

    [<Fact>]
    let ``zeroCreate`` () = Assert.Equal<int[,]>(array2D [|[|0;0;0|]; [|0;0;0|]|], TS.zeroCreate 2 3 |> TS.data)

    [<Fact>]
    let ``sub`` () = Assert.Equal<int[,]>(array2D [|[|1;2|];[|4;5|]|], TS.sub 1 2 ts |> TS.data)

    let timeValues:obj[] list = [ [|0;[|0;3|]|]; [|-1;[|0;0|]|]; [|3;[|0;0|]|] ]
    [<Theory>]
    [<MemberData(nameof(timeValues))>]
    let ``atTime`` t expected = Assert.Equal<int[]>(expected, TS.atTime t ts)
        





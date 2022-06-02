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
        


(*    [<Fact>]
    let ``Get current index`` () =  
        let expected = 1
        let actual = State.eval (TimeseriesState.currentTime) initialState
        Assert.Equal<int>(expected,actual)

    [<Fact>]
    let ``Get current element`` () = 
        let expected = [|Some 1f;Some 51f;Some 101f;Some 151f;Some 201f|]
        let actual = State.eval (TimeseriesState.currentElements) initialState
        Assert.Equal<float32 option []>(expected,actual)

    [<Fact>]
    let ``Get element at lag 1``() = 
        let expected = [|Some 0f;Some 50f;Some 100f;Some 150f;Some 200f|]
        let actual = State.eval (TimeseriesState.lagElements 1) initialState
        Assert.Equal<float32 option[]>(expected,actual)

    [<Fact>]
    let ``Modify current index``() = 
        let expected = 5
        let m = monad { do! TimeseriesState.setTime 5
                        return! TimeseriesState.currentTime }
        let actual = State.eval m initialState
        Assert.Equal<int>(expected,actual)*)


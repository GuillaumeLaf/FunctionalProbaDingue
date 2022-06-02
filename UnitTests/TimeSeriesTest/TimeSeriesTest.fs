namespace TimeseriesTest

module TS =
    open Xunit
    open Timeseries
    open Timeseries.TimeseriesType
    open FSharpPlus.Data
    open FSharpPlus

    let ts = Array2D.init 2 3 (fun i j -> i*3 + j) |> TS.create
    let initialState : (int * TS<int>) = (1,ts) 

    [<Fact>]
    let ``dataDefault`` () = Assert.Equal<int[,]>(array2D [|[|0;1;2|];[|3;4;5|]|], TS.dataDefault ts) 
        
    [<Fact>]
    let ``pctLength`` () = Assert.Equal<int>(1, TS.pctLength 50f ts)

    [<Fact>]
    let ``zeroCreate`` () = Assert.Equal<TS<int>>(array2D >> TS.create <| [|[|0;0;0|]; [|0;0;0|]|], TS.zeroCreate 2 3)

    [<Fact>]
    let ``sub`` () = Assert.Equal<TS<int>>((array2D >> TS.create) <| [|[|1;2|];[|4;5|]|], TS.sub 1 2 ts)

    [<Theory>]
    [<InlineData(0,[|0;3|])>]
    let ``atTime`` (t:int) (expected:int[]) = 
        


    [<Fact>]
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
        Assert.Equal<int>(expected,actual)


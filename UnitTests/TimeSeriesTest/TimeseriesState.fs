namespace TimeseriesTest

module TimeseriesState =
    open Xunit
    open Timeseries
    open Timeseries.TimeseriesType
    open Timeseries.TimeseriesState
    open FSharpPlus.Data
    open FSharpPlus

    let ts = Array2D.init 2 3 (fun i j -> i*3 + j) |> TS.create
    let initialState:int*TS<int> = (1,ts)

    [<Fact>]
    let ``currentTime`` () = 
        let expected = 1
        let actual = State.eval (TimeseriesState.currentTime()) initialState
        Assert.Equal<int>(expected,actual)

    [<Fact>]
    let ``setTime``() = 
        let expected = 2
        let m = monad { do! TimeseriesState.setTime 2
                        return! TimeseriesState.currentTime() }
        let actual = State.eval m initialState
        Assert.Equal<int>(expected,actual)

    [<Fact>]
    let ``currentElements`` () = 
        let expected = [| 1; 4|]
        let actual = State.eval (TimeseriesState.currentElements()) initialState
        Assert.Equal<int[]>(expected,actual)

    [<Fact>]
    let ``lagElements 1``() = 
        let expected = [| 0; 3|]
        let actual = State.eval (TimeseriesState.lagElements 1) initialState
        Assert.Equal<int[]>(expected,actual)

    [<Fact>]
    let ``multipleLagElementsFor`` () = 
        let expected = [| 0; 1|]
        let actual = State.eval (TimeseriesState.multipleLagElementsFor 1 0) initialState
        Assert.Equal<int[]>(expected, actual)

    [<Fact>]
    let ``multipleLagElements`` () = 
        let expected = array2D [|[| 0; 1|]; [| 3; 4|]|]
        let actual = State.eval (TimeseriesState.multipleLagElements 1) initialState
        Assert.Equal<int[,]>(expected, actual)











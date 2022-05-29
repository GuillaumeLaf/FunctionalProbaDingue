﻿module TimeSeriesTest
    open Xunit
    open Timeseries
    open Timeseries.TimeseriesType
    open Timeseries.TimeseriesState
    open FSharpPlus.Data
    open FSharpPlus

    let ts = Array2D.init 5 50 (fun i j -> i*50 + j |> (float32 >> Some)) |> TS.create
    let initialState = (1,ts) : (int * TS<float32 option>)

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


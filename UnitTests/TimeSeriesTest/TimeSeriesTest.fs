﻿module TimeSeriesTest
    open Xunit
    open TimeSeries
    open FSharpPlus.Data
    open FSharpPlus

    let initialState() = (1,Multivariate.TS(Array2D.init 5 50 (fun i j -> i*50 + j |> float32))) : (int * Multivariate.TS)

    [<Fact>]
    let ``Get current index`` () =  
        let expected = 1
        let actual = State.eval (Multivariate.currentTime) (initialState())
        Assert.Equal<int>(expected,actual)

    [<Fact>]
    let ``Get current element`` () = 
        let expected = Some [|1f;51f;101f;151f;201f|]
        let actual = State.eval (Multivariate.currentElements) (initialState())
        Assert.Equal<float32[] option>(expected,actual)

    [<Fact>]
    let ``Get element at lag 1``() = 
        let expected = Some [|0f;50f;100f;150f;200f|]
        let actual = State.eval (Multivariate.lagElements 1) (initialState())
        Assert.Equal<float32[] option>(expected,actual)

    [<Fact>]
    let ``Modify current index``() = 
        let expected = 5
        let m = monad { do! Multivariate.setTime 5
                        return! Multivariate.currentTime }
        let actual = State.eval m (initialState())
        Assert.Equal<int>(expected,actual)

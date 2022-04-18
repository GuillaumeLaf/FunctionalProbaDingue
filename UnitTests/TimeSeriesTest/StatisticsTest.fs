module StatisticsTest
    open Xunit
    open Timeseries.TimeseriesType
    open Timeseries    
    open FSharpPlus
    open FSharpPlus.Control
    open FSharpPlus.Data
    open MathNet.Numerics.LinearAlgebra

        
    let ts = Array2D.ofArray [|[|2f;1f|]; [|1f;2f|]|] |> TS.create

    [<Fact>]
    let ``Means computation`` () = 
        let actual = Stats.mean ts
        let expected = Array.create 2 1.5f
        Assert.Equal<float32 []>(expected, actual)

    [<Fact>]
    let ``Stds computation`` () = 
        let actual = Stats.std ts
        let expected = Array.create 2 (1f/sqrt(2f)) 
        Assert.Equal<float32[]>(expected, actual)

(*    [<Fact>]
    let ``Vars computation`` () = 
        let actual = Stats.onlyVar ts
        let expected = [|0.5f; 0.5f|]
        Assert.Equal<float32[]>(expected, actual)*)

    [<Fact>]
    let ``Covs computation`` () = 
        let actual = Stats.cov ts
        let expected = Array2D.ofArray [|[|1f/2f;-0.5f|];[|-0.5f;1f/2f|]|]
        Assert.Equal<float32[,]>(expected, actual) 

    [<Fact>]
    let ``Cholesky Lower computation`` () = 
        let actual = Stats.cholesky ts
        let expected = Array2D.ofArray [|[|0.7071067691f; 0.0f|];[|-0.7071067691f; 0.0001726334885f|]|]
        Assert.Equal<float32[,]>(expected, actual)






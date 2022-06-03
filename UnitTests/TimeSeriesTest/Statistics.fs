namespace TimeseriesTest

module Statistics =
    open Xunit
    open Timeseries.TimeseriesType
    open Timeseries    
    open FSharpPlus
    open FSharpPlus.Control
    open FSharpPlus.Data
    open MathNet.Numerics.LinearAlgebra

    let ts = array2D >> TS.create <| [|[|2f;1f|]; [|1f;2f|]|] 

    [<Fact>]
    let ``mean`` () = 
        let actual = Stats.mean ts
        let expected = Array.create 2 1.5f
        Assert.Equal<float32 []>(expected, actual)

    [<Fact>]
    let ``var`` () = 
        let actual = Stats.var ts
        let expected = [|0.5f; 0.5f|]
        Assert.Equal<float32[]>(expected, actual)

    [<Fact>]
    let ``cov`` () = 
        let actual = Stats.cov ts
        let expected = array2D [|[|1f/2f;-0.5f|];[|-0.5f;1f/2f|]|]
        Assert.Equal<float32[,]>(expected, actual) 

    [<Fact>]
    let ``cholesky`` () = 
        let actual = Stats.cholesky ts
        let expected = array2D [|[|0.7071067691f; 0.0f|];[|-0.7071067691f; 0.0001726334885f|]|]
        Assert.Equal<float32[,]>(expected, actual)






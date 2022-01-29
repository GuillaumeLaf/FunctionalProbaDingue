module StatisticsTest
    open Xunit
    open TimeSeries.Statistics
    open FSharpPlus.Data
    open FSharpPlus
    open MathNet.Numerics.LinearAlgebra

    module Univariate = 
        
        let ts = Array.init 10 (id >> float32)

        [<Fact>]
        let ``Mean computation`` () = 
            let s = Univariate.Stats(ts)
            let actual = s.Mean
            let expected = 4.5f
            Assert.Equal<float32>(expected, actual)

        [<Fact>]
        let ``Std computation`` () = 
            let s = Univariate.Stats(ts)
            let actual = s.Std
            let expected = sqrt (55f/6f)
            Assert.Equal<float32>(expected, actual)

        [<Fact>]
        let ``Var computation`` () = 
            let s = Univariate.Stats(ts)
            let actual = s.Var
            let expected = float32 (55f/6f)
            Assert.Equal<float32>(expected, actual)

    module Multivariate = 
        
        let ts = Array2D.ofArray [|[|2f;1f|]; [|1f;2f|]|]

        [<Fact>]
        let ``Means computation`` () = 
            let s = Multivariate.Stats(ts)
            let actual = s.Means
            let expected = Array.create 2 1.5f
            Assert.Equal<float32[]>(expected, actual)

        [<Fact>]
        let ``Stds computation`` () = 
            let s = Multivariate.Stats(ts)
            let actual = s.Stds
            let expected = Array.create 2 (1f/sqrt(2f))
            Assert.Equal<float32[]>(expected, actual)

(*        [<Fact>]
        let ``Vars computation`` () = 
            let s = Multivariate.Stats(ts)
            let actual = s.Vars
            let expected = [|1f/2f; 1f/2f|]
            Assert.Equal<float32[]>(expected, actual)*)

        [<Fact>]
        let ``Covs computation`` () = 
            let s = Multivariate.Stats(ts)
            let actual = s.Covs
            let expected = Array2D.ofArray [|[|1f/2f;-0.5f|];[|-0.5f;1f/2f|]|]
            Assert.Equal<float32[,]>(expected, actual)

        [<Fact>]
        let ``Cholesky Lower computation`` () = 
            let s = Multivariate.Stats(ts)
            let actual = s.CholeskyLowerCovs
            let expected = Array2D.ofArray [|[|sqrt 2f; 0f|];[|1f/sqrt(2f); sqrt(3f/2f)|]|]
            Assert.Equal<float32[,]>(expected, actual)

        [<Fact>]
        let ``Does not have Covariances`` () = 
            let s = Multivariate.Stats(ts)
            let actual = s.HasCovs
            Assert.True(not actual)
        
        [<Fact>]
        let ``Add Covariance matrix`` () = 
            let s = Multivariate.Stats(ts)
            s.AddCovs (Array2D.ofArray [|[|1f/2f;-0.5f|];[|-0.5f;1f/2f|]|])
            Assert.True(s.HasCovs)






module TimeSeriesTest
    open Xunit
    open TimeSeries
    open FSharpPlus.Data
    open FSharpPlus

    let initialState() = (1,Multivariate.TS(Array2D.init 5 50 (fun i j -> i*50 + j))) : (int * Multivariate.TS<int>)

    [<Fact>]
    let ``Get current index`` () =  
        let expected = 1
        let actual = State.eval (Multivariate.currentTime()) (initialState())
        Assert.Equal<int>(expected,actual)

    [<Fact>]
    let ``Get current element`` () = 
        let expected = Some [|1;51;101;151;201|]
        let actual = State.eval (Multivariate.currentElements()) (initialState())
        Assert.Equal<int[] option>(expected,actual)

    [<Fact>]
    let ``Get element at lag 1``() = 
        let expected = Some [|0;50;100;150;200|]
        let actual = State.eval (Multivariate.lagElements 1) (initialState())
        Assert.Equal<int[] option>(expected,actual)

    [<Fact>]
    let ``Modify current index``() = 
        let expected = 5
        let actual = State.eval (Multivariate.setIndex 5 >>= Multivariate.currentTime) (initialState())
        Assert.Equal<int>(expected,actual)


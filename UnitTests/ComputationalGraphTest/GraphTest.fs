module GraphTest
    open Xunit
    open ComputationalGraph.Graph
    open FSharpPlus.Data
    open FSharpPlus

    [<Fact>]
    let ``run graph and treat indices as data`` () = 
        let g : Graph<int> = DataPoint(Parameter(0)) + DataPoint(Parameter(1)) + DataPoint(Parameter(2)) * DataPoint(Parameter(3))
        let initialState = Monad.S([|0;1;2;3|], [|0|], [|0|])
        let result = Graph<int>.run g
        let expected = 7
        Assert.Equal<int>(expected, result)

    [<Fact>]
    let ``Create State monad with graph and runs it`` () = 
        let (g:Graph<int>) = DataPoint(Parameter(0)) + DataPoint(Parameter(1)) + DataPoint(Parameter(2)) * DataPoint(Parameter(3))
        let initialState : Monad.S<int> = Monad.S([|0;1;2;3|], [|0|], [|0|])
        let m = Graph<int>.ToMonad g
        let expected = 7
        let actual = State.eval m initialState
        Assert.Equal<int>(expected, actual)

    [<Fact>]
    let ``Humongous graph to check StackOverflow`` () = 
        let g = Seq.init 10000 (fun _ -> DataPoint(Parameter(0))) |> sum
        let initialState = Monad.S([|0|], [|0|], [|0|])
        let m = Graph<int>.ToMonad g
        let expected = 0
        let actual = State.eval m initialState
        Assert.Equal<int>(expected, actual)

    [<Fact>]
    let ``Count the number of Input nodes in a graph`` () = 
        let g = DataPoint(Parameter(0)) + DataPoint(Variable(0)) + DataPoint(Parameter(1)) * DataPoint(Variable(1))
        let expected = [|Parameter(2); Variable(2)|]
        let actual = Graph<int>.size g
        Assert.Equal<Input[]>(expected,actual)


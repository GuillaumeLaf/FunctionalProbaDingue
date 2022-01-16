module GraphTest
    open Xunit
    open ComputationalGraph.Graph
    open FSharpPlus.Data
    open FSharpPlus

    [<Fact>]
    let ``run graph and treat indices as data`` () = 
        let g : Graph = Input(Parameter(0,0)) + Input(Parameter(0,1)) + Input(Parameter(0,2)) * Input(Parameter(0,3))
        let initialState = Monad.S([|[|0;1;2;3|]|], [|[|0|]|], [|[|0|]|])
        let result = Graph.run g
        let expected = 7f
        Assert.Equal<float32>(expected, result)

    [<Fact>]
    let ``Create State monad with graph and runs it`` () = 
        let (g:Graph) = Input(Parameter(0,0)) + Input(Parameter(0,1)) + Input(Parameter(0,2)) * Input(Parameter(0,3))
        let initialState = Monad.S([|[|0f;1f;2f;3f|]|], [|[|0f|]|], [|[|0f|]|])       : Monad.S<float32>
        let m = Graph.ToMonad g
        let expected = 7f
        let actual = State.eval m initialState
        Assert.Equal<float32>(expected, actual)

    [<Fact>]
    let ``Humongous graph to check StackOverflow`` () = 
        let g = Seq.init 10000 (fun _ -> Input(Parameter(0,0))) |> sum
        let initialState = Monad.S([|[|0f|]|], [|[|0f|]|], [|[|0f|]|])         : Monad.S<float32>
        let m = Graph.ToMonad g
        let expected = 0f
        let actual = State.eval m initialState
        Assert.Equal<float32>(expected, actual)

    [<Fact>]
    let ``Count the number of Input nodes in a graph by group`` () = 
        let g = Input(Parameter(0,0)) + Input(Variable(1,0)) + Input(Parameter(0,1)) * Input(Variable(0,1))
        let expected = [|Parameter(0,2); Variable(0,1); Variable(1,1)|]
        let actual = Graph.groupSizes g
        Assert.Equal<BasicInput[]>(expected,actual)


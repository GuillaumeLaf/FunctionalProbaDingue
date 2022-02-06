module GraphTest
    open Xunit
    open ComputationalGraph.GraphType
    open ComputationalGraph
    open FSharpPlus.Data
    open FSharpPlus

    // P0 + P2 + (P2 * P3)
    let graph = Graph.add (Input(Parameter(0,0))) (Input(Parameter(0,1))) |> Graph.add (Graph.multiply (Input(Parameter(0,2))) (Input(Parameter(0,3))))
    let initialState = S(Array2D.ofArray [|[|0f;1f;2f;3f|]|], Array2D.ofArray [|[|0f|]|], Array2D.ofArray [|[|0f|]|])

    [<Fact>]
    // 'run'
    let ``run graph and treat indices as data`` () = 
        let result = Graph.run graph
        let expected = 7f
        Assert.Equal<float32>(expected, result)

    [<Fact>]
    // 'ToMonad'
    let ``Create State monad with graph and runs it`` () = 
        let m = Graph.toMonad graph
        let expected = 7f
        let actual = State.eval m initialState
        Assert.Equal<float32>(expected, actual)

    [<Fact>]
    let ``Humongous graph to check StackOverflow`` () = 
        let g = Array.init 10000 (fun _ -> Input(Parameter(0,0))) |> Array.reduce Graph.add
        let initialState = S(Array2D.ofArray [|[|0f|]|], Array2D.ofArray [|[|0f|]|], Array2D.ofArray [|[|0f|]|])        
        let m = Graph.toMonad g
        let expected = 0f
        let actual = State.eval m initialState
        Assert.Equal<float32>(expected, actual)

    [<Fact>]
    // 'groupSizes'
    let ``Count the number of Input nodes in a graph by group`` () = 
        let graph = [| Input(Parameter(0,0));Input(Variable(1,0)); Graph.multiply (Input(Parameter(0,1))) (Input(Variable(0,1))) |] |> Array.reduce Graph.add
        let expected = [|Parameter(0,2); Variable(0,1); Variable(1,1)|]
        let actual = Graph.groupSizes graph
        Assert.Equal<BasicInput[]>(expected,actual)

    [<Fact>]
    // 'op_Equality'
    let ``Are Graphs equals ?`` () = 
        let graph = [| Input(Parameter(0,0));Input(Variable(1,0)); Graph.multiply (Input(Parameter(0,1))) (Input(Variable(0,1))) |] |> Array.reduce Graph.add
        Assert.True(Graph.equal graph graph)

    [<Fact>]
    // 'collectSubGraphs'
    let ``Collect every subGraphs in an array`` () = 
        let graph = Graph.multiply (Input(Parameter(0,1))) (Input(Variable(0,1))) |> Graph.add (Input(Parameter(0,0)))
        let expected = [| graph; 
                          Input(Parameter(0,0)); Graph.multiply (Input(Parameter(0,1))) (Input(Variable(0,1))); 
                          Input(Parameter(0,1)) ; Input(Variable(0,1)) |]
        let actual = Graph.collectSubGraphs graph
        Assert.Equal<Graph[]>(expected, actual)
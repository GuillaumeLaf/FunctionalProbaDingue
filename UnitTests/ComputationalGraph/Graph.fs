namespace GraphTest

module Graph =
    open Xunit
    open ComputationalGraph.GraphType
    open ComputationalGraph
    open FSharpPlus.Data
    open FSharpPlus

    // P00 + P01 + (P02 * P03)
    let graph = Graph.add (Input(Parameter(0,0))) (Input(Parameter(0,1))) |> Graph.add (Graph.multiply (Input(Parameter(0,2))) (Input(Parameter(0,3))))
    let initialState = S(array2D [|[|0f;1f;2f;3f|]|], array2D [|[|0f|]|], array2D [|[|0f|]|])

    [<Fact>]
    let ``ToMonad`` () = 
        let m = Graph.toMonad graph
        let expected = 7f
        let actual = State.eval m initialState
        Assert.Equal<float32>(expected, actual)

    [<Fact>]
    let ``Humongous graph to check StackOverflow`` () = 
        let g = Array.init 10000 (fun _ -> Input(Parameter(0,0))) |> Array.reduce Graph.add
        let initialState = S(array2D [|[|0f|]|], array2D [|[|0f|]|], array2D [|[|0f|]|])        
        let m = Graph.toMonad g
        let expected = 0f
        let actual = State.eval m initialState
        Assert.Equal<float32>(expected, actual)

    [<Fact>]
    let ``Count`` () = 
        let graph = [| Input(Parameter(0,0));Input(Variable(1,0)); Graph.multiply (Input(Parameter(0,1))) (Input(Variable(0,1))) |] |> Array.reduce Graph.add
        let expected = [|Parameter(0,2); Variable(0,1); Variable(1,1)|]
        let actual = Graph.count graph
        Assert.Equal<BasicInput[]>(expected,actual)

    [<Fact>]
    let ``CountUnique`` () = 
        let graph = [| Input(Parameter(0,0));Input(Variable(1,0)); Graph.multiply (Input(Parameter(0,1))) (Input(Variable(0,1))) |] |> Array.reduce Graph.add
        let expected = [|Parameter(0,2); Variable(0,1); Variable(1,1)|]
        let actual = Graph.count graph
        Assert.Equal<BasicInput[]>(expected,actual)

    [<Fact>]
    let ``Equal`` () = 
        let graph = [| Input(Parameter(0,0));Input(Variable(1,0)); Graph.multiply (Input(Parameter(0,1))) (Input(Variable(0,1))) |] |> Array.reduce Graph.add
        Assert.True(Graph.equal graph graph)

    [<Fact>]
    let ``CollectSubGraphs`` () = 
        let graph = Graph.multiply (Input(Parameter(0,1))) (Input(Variable(0,1))) |> Graph.add (Input(Parameter(0,0)))
        let expected = [| graph; 
                          Input(Parameter(0,0)); Graph.multiply (Input(Parameter(0,1))) (Input(Variable(0,1))); 
                          Input(Parameter(0,1)) ; Input(Variable(0,1)) |]
        let actual = Graph.collectSubGraphs graph
        Assert.Equal<Graph<obj>[]>(expected, actual)

    [<Fact>]
    let ``countGroups`` () = 
        let expected = [|0|]
        let actual = Graph.countGroups graph
        Assert.Equal<int[]>(expected, actual)

    [<Fact>]
    let ``ChangeGroup`` () = 
        let expected : Graph<float32> = Graph.add (Input(Parameter(1,0))) (Input(Parameter(1,1))) |> Graph.add (Graph.multiply (Input(Parameter(1,2))) (Input(Parameter(1,3))))
        let actual = Graph.changeGroup 0 1 graph
        Assert.Equal<Graph<float32>>(expected, actual)

    [<Fact>]
    let ``Shift`` () = 
        let expected:Graph<float32> = Graph.add (Input(Parameter(0,2))) (Input(Parameter(0,3))) |> Graph.add (Graph.multiply (Input(Parameter(0,4))) (Input(Parameter(0,5))))
        let actual = Graph.shift Parameter 2 graph
        Assert.Equal<Graph<float32>>(expected, actual)

    [<Fact>]
    let ``DefaultState`` () = 
        let expected = GraphType.S(array2D [|[|0f;0f;0f;0f|]|], array2D [||], array2D [||])
        let actual:S<float32> = Graph.defaultState graph
        Assert.Equal<S<float32>>(expected, actual)

        





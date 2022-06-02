namespace SimplifyTest

module Addition =
    open Xunit
    open ComputationalGraph.GraphType
    open ComputationalGraph
    open FSharpPlus.Data
    open FSharpPlus  

    [<Fact>]
    let ``Addition(0,r)`` () =
        let graph = Constant(0) + Input(Parameter(0,0))*Input(Variable(0,0)) 
        let expected = Input(Parameter(0,0))*Input(Variable(0,0)) 
        let actual = Graph.simplify graph
        Assert.Equal<Graph<int>>(expected, actual)

    [<Fact>]
    let ``Addition(l,0)`` () =
        let graph = Input(Parameter(0,0))*Input(Variable(0,0)) + Constant(0) 
        let expected = Input(Parameter(0,0))*Input(Variable(0,0)) 
        let actual = Graph.simplify graph
        Assert.Equal<Graph<int>>(expected, actual)


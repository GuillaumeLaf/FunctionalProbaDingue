namespace SimplifyTest

module Polynomial =
    open Xunit
    open ComputationalGraph.GraphType
    open ComputationalGraph
    open FSharpPlus.Data
    open FSharpPlus

    [<Fact>]
    let ``Polynomial(_,0)`` () =
        // (V00*P00 + V01*P01) ** 0
        let graph :Graph<int> = Polynomial(Input(Parameter(0,0))*Input(Variable(0,0)) + Input(Parameter(0,1))*Input(Variable(0,1)), 0)
        let expected = Constant(1)
        let actual = Graph.simplify graph
        Assert.Equal<Graph<int>>(expected, actual)

    [<Fact>]
    let ``Polynomial(_,1)`` () =
        // (V00*P00 + V01*P01) ** 0
        let graph :Graph<int> = Polynomial(Input(Parameter(0,0))*Input(Variable(0,0)) + Input(Parameter(0,1))*Input(Variable(0,1)), 1)
        let expected = Input(Parameter(0,0))*Input(Variable(0,0)) + Input(Parameter(0,1))*Input(Variable(0,1))
        let actual = Graph.simplify graph
        Assert.Equal<Graph<int>>(expected, actual)

    [<Fact>]
    let ``Polynomial(0,_)`` () =
        // (V00*P00 + V01*P01) ** 0
        let graph :Graph<int> = Polynomial(Constant(0),10)
        let expected = Constant(0)
        let actual = Graph.simplify graph
        Assert.Equal<Graph<int>>(expected, actual)

    [<Fact>]
    let ``Polynomial(1,_)`` () =
        // (V00*P00 + V01*P01) ** 0
        let graph :Graph<int> = Polynomial(Constant(1),10)
        let expected = Constant(1)
        let actual = Graph.simplify graph
        Assert.Equal<Graph<int>>(expected, actual)

    [<Fact>]
    let ``Polynomial(Polynomial(_,a),b)`` () =
        // (V00*P00 + V01*P01) ** 0
        let graphInner :Graph<int> = Polynomial(Input(Parameter(0,0))*Input(Variable(0,0)) + Input(Parameter(0,1))*Input(Variable(0,1)), 3)
        let graph : Graph<int> = Polynomial(graphInner, 2)
        let expected = Polynomial(Input(Parameter(0,0))*Input(Variable(0,0)) + Input(Parameter(0,1))*Input(Variable(0,1)), 6)
        let actual = Graph.simplify graph
        Assert.Equal<Graph<int>>(expected, actual)

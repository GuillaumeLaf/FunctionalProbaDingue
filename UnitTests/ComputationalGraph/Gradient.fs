namespace GradientTest

module Gradient = 
    open Xunit
    open ComputationalGraph.GraphType
    open ComputationalGraph
    open FSharpPlus.Data
    open FSharpPlus

    [<Fact>]
    let ``Constant`` () = 
        let graph = Constant(2)
        let expected = Constant(0)
        let actual = Graph.gradientForParameter 0 0 graph
        Assert.Equal<Graph<int>>(expected, actual)

    [<Fact>]
    let ``Polynomial`` () = 
        let graph = Polynomial(Input(Parameter(0,0))*Input(Variable(0,0)) + Input(Parameter(0,1))*Input(Variable(0,1)), 3)
        let expected = Input(Variable(0,0)) * Graph.Constant(3) * Polynomial(Input(Parameter(0,0))*Input(Variable(0,0)) + Input(Parameter(0,1))*Input(Variable(0,1)), 2)
        let actual = Graph.gradientForParameter 0 0 graph
        Assert.Equal<Graph<int>>(expected, actual)

    [<Fact>]
    let ``Addition`` () = 
        let graph = Input(Parameter(0,0)) + Input(Parameter(0,1))*Input(Variable(0,1))
        let expected = Graph.Constant(1)
        let actual = Graph.gradientForParameter 0 0 graph
        Assert.Equal<Graph<int>>(expected, actual)

    [<Fact>]
    let ``Substraction`` () = 
        let graph = Input(Parameter(0,1))*Input(Variable(0,1)) - Input(Parameter(0,0))
        let expected = Graph.Constant(-1)
        let actual = Graph.gradientForParameter 0 0 graph
        Assert.Equal<Graph<int>>(expected, actual)  

    [<Fact>]
    let ``Multiplication`` () = 
        let graph = Input(Parameter(0,0))*Input(Variable(0,0)) + Input(Parameter(0,1))*Input(Variable(0,1))
        let expected = Input(Variable(0,0))
        let actual = Graph.gradientForParameter 0 0 graph
        Assert.Equal<Graph<int>>(expected, actual)  

    [<Fact>]
    let ``Parameter`` () = 
        let graph = Input(Parameter(0,0))
        let expected = Graph.Constant(1)
        let actual = Graph.gradientForParameter 0 0 graph
        Assert.Equal<Graph<int>>(expected, actual)  

    [<Fact>]
    let ``Variable`` () = 
        let graph = Input(Variable(0,0))
        let expected = Graph.Constant(0)
        let actual = Graph.gradientForParameter 0 0 graph
        Assert.Equal<Graph<int>>(expected, actual)  

    [<Fact>]
    let ``Innovation`` () = 
        let graph = Input(Innovation(0,0))
        let expected = Graph.Constant(0)
        let actual = Graph.gradientForParameter 0 0 graph
        Assert.Equal<Graph<int>>(expected, actual) 
        
    [<Fact>]
    let ``gradientForGroup`` () = 
        let graph = Input(BasicInput.Parameter(0,0))*Graph.Input(BasicInput.Variable(0,0)) + Input(BasicInput.Parameter(0,1))*Input(BasicInput.Variable(0,1))
        let expected = [| Graph.Input(BasicInput.Variable(0,0)); Input(BasicInput.Variable(0,1)) |]
        let actual = Graph.gradientForGroup 0 graph
        Assert.Equal<Graph<int>[]>(expected, actual)

    [<Fact>]
    let ``gradient`` () = 
        let graph:Graph<int>[] = [| Input(BasicInput.Parameter(0,0))*Graph.Input(BasicInput.Variable(0,0)); Input(BasicInput.Parameter(1,0))*Input(BasicInput.Variable(1,0)) |]
        let expected = array2D [| [| Graph.Input(BasicInput.Variable(0,0)) |]; [| Input(BasicInput.Variable(1,0)) |] |]
        let actual = Graph.gradient graph
        Assert.Equal<Graph<int>[,]>(expected, actual)









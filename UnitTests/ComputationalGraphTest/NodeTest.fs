module NodeTest
    open Xunit
    open ComputationalGraph
    open ComputationalGraph.GraphType
    open FSharpPlus.Data
    open FSharpPlus

    [<Fact>]
    let ``Create a new Parameter Vector of length 2`` () = 
        let actual = Node.Vector.init Parameter (Node.Group(0)) 2
        let expected = Node.Vector.create 2 [| Input(Parameter(0,0)); Input(Parameter(0,1)) |]
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Add two Vectors`` () = 
        let v1 = Node.Vector.init Parameter (Node.Group(0)) 2
        let v2 = Node.Vector.init Variable (Node.Group(0)) 2
        let actual = Node.Vector.add v1 v2
        let expected = Node.Vector.create 2 [| Input(Parameter(0,0)) + Input(Variable(0,0)); Input(Parameter(0,1)) + Input(Variable(0,1)) |]
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Multiply two Vectors`` () = 
        let v1 = Node.Vector.init Parameter (Node.Group(0)) 2
        let v2 = Node.Vector.init Variable (Node.Group(0)) 2
        let actual = v1 * v2
        let expected = Node.Vector.create 2 [| Input(Parameter(0,0)) * Input(Variable(0,0)); Input(Parameter(0,1)) * Input(Variable(0,1)) |]
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``DotProduct of two Vectors`` () = 
        let v1 = Node.Vector.init Parameter (Node.Group(0)) 2
        let v2 = Node.Vector.init Variable (Node.Group(0)) 2
        let actual = Node.Vector.dotProduct v1 v2
        let expected = Input(Parameter(0,0)) * Input(Variable(0,0)) + Input(Parameter(0,1)) * Input(Variable(0,1))
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Create a new Parameter Matrix of dimensions (2,3)`` () = 
        let actual = Node.Matrix.init Parameter (2,3)
        let newVector grp = Node.Vector.init Parameter (Node.Group(grp)) 2
        let expected = Node.Matrix.create (2,3) [| newVector 0 ; newVector 1 ; newVector 2 |]
        Assert.Equal<Matrix>(expected, actual)

    [<Fact>]
    let ``Add two Matrices`` () = 
        let m1 = Matrix.init Parameter (2,3)
        let m2 = Matrix.init Parameter (2,3)
        let actual = m1 + m2
        let newVector grp = [| Input(Parameter(grp,0))+Input(Parameter(grp,0)); Input(Parameter(grp,1))+Input(Parameter(grp,1)) |] |> Vector
        let expected = [| newVector 0; newVector 1; newVector 2 |] |> Matrix
        Assert.Equal<Matrix>(expected, actual)

    [<Fact>]
    let ``Multiply two Matrices`` () = 
        let m1 = Matrix.init Parameter (2,3)
        let m2 = Matrix.init Parameter (2,3)
        let actual = m1 * m2
        let newVector grp = [| Input(Parameter(grp,0))*Input(Parameter(grp,0)); Input(Parameter(grp,1))*Input(Parameter(grp,1)) |] |> Vector
        let expected = [| newVector 0; newVector 1; newVector 2 |] |> Matrix
        Assert.Equal<Matrix>(expected, actual)

    [<Fact>]
    let ``DotProduct of a Vector and a Matrix`` () = 
        let v = Vector.init Parameter (Group(0)) 2
        let m = Matrix.init Parameter (2,3)
        let actual = v */ m
        let expected = [| Input(Parameter(0,0))*Input(Parameter(0,0))+Input(Parameter(0,1))*Input(Parameter(0,1)); 
                          Input(Parameter(1,0))*Input(Parameter(0,0))+Input(Parameter(1,1))*Input(Parameter(0,1));
                          Input(Parameter(2,0))*Input(Parameter(0,0))+Input(Parameter(2,1))*Input(Parameter(0,1))|] |> Vector
        Assert.Equal<Vector>(expected, actual)

    [<Fact>]
    let ``DotProduct of a Matrix and a Vector`` () = 
        let v = Vector.init Parameter (Group(0)) 2
        let m = Matrix.init Parameter (3,2)
        let actual = m */ v
        let expected = [| Input(Parameter(0,0))*Input(Parameter(0,0))+Input(Parameter(1,0))*Input(Parameter(0,1)); 
                          Input(Parameter(0,1))*Input(Parameter(0,0))+Input(Parameter(1,1))*Input(Parameter(0,1));
                          Input(Parameter(0,2))*Input(Parameter(0,0))+Input(Parameter(1,2))*Input(Parameter(0,1))|] |> Vector
        Assert.Equal<Vector>(expected, actual)

    [<Fact>]
    let ``DotProduct of two Matrices`` () = 
        let m1 = Matrix.init Parameter (2,3)
        let m2 = Matrix.init Parameter (3,2)
        let actual = m1 */ m2
        let a11 = Input(Parameter(0,0))*Input(Parameter(0,0))+Input(Parameter(0,1))*Input(Parameter(1,0))+Input(Parameter(0,2))*Input(Parameter(2,0))
        let a12 = Input(Parameter(1,0))*Input(Parameter(0,0))+Input(Parameter(1,1))*Input(Parameter(1,0))+Input(Parameter(1,2))*Input(Parameter(2,0))
        let a21 = Input(Parameter(0,0))*Input(Parameter(0,1))+Input(Parameter(0,1))*Input(Parameter(1,1))+Input(Parameter(0,2))*Input(Parameter(2,1))
        let a22 = Input(Parameter(1,0))*Input(Parameter(0,1))+Input(Parameter(1,1))*Input(Parameter(1,1))+Input(Parameter(1,2))*Input(Parameter(2,1))
        let expected = [| Vector([|a11;a21|]); Vector([|a12;a22|]) |] |> Matrix
        Assert.Equal<Matrix>(expected, actual)

    [<Fact>]
    let ``Univariate Linear Combinaison`` () = 
        let actual = linearCombinaison 0 1 2
        let expected = Input(Parameter(0,1))*Input(Variable(0,1))+Input(Parameter(0,2))*Input(Variable(0,2))
        Assert.Equal<Graph>(expected, actual)
    
    [<Fact>]
    let ``Multivariate Linear Combinaison`` () = 
        let actual = multivariateLinearCombinaison 1 2 1
        let expected = [|Input(Parameter(0,0))*Input(Variable(0,1))+Input(Parameter(0,1))*Input(Variable(0,2))|]
        Assert.Equal<Graph[]>(expected, actual)
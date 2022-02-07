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
        let actual = v1 + v2
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
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Add two Matrices`` () = 
        let m1 = Node.Matrix.init Parameter (2,3)
        let m2 = Node.Matrix.init Parameter (2,3)
        let actual = m1 + m2
        let newVector grp = Node.Vector.createFrom [| Input(Parameter(grp,0))+Input(Parameter(grp,0)); Input(Parameter(grp,1))+Input(Parameter(grp,1)) |]
        let expected = Node.Matrix.create (2,3) [| newVector 0; newVector 1; newVector 2 |]
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Multiply two Matrices`` () = 
        let m1 = Node.Matrix.init Parameter (2,3)
        let m2 = Node.Matrix.init Parameter (2,3)
        let actual = m1 * m2
        let newVector grp = Node.Vector.createFrom [| Input(Parameter(grp,0))*Input(Parameter(grp,0)); Input(Parameter(grp,1))*Input(Parameter(grp,1)) |]
        let expected = Node.Matrix.create (2,3) [| newVector 0; newVector 1; newVector 2 |]
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``DotProduct of a Vector and a Matrix`` () = 
        let v = Node.Vector.init Parameter (Node.Group(0)) 2
        let m = Node.Matrix.init Parameter (2,3)
        let actual = Node.Matrix.leftVectorProduct v m
        let expected = Node.Vector.createFrom [| Input(Parameter(0,0))*Input(Parameter(0,0))+Input(Parameter(0,1))*Input(Parameter(0,1)); 
                                                 Input(Parameter(1,0))*Input(Parameter(0,0))+Input(Parameter(1,1))*Input(Parameter(0,1));
                                                 Input(Parameter(2,0))*Input(Parameter(0,0))+Input(Parameter(2,1))*Input(Parameter(0,1))|]
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``DotProduct of a Matrix and a Vector`` () = 
        let v = Node.Vector.init Parameter (Node.Group(0)) 2
        let m = Node.Matrix.init Parameter (3,2)
        let actual = Node.Matrix.rightVectorProduct m v
        let expected = Node.Vector.createFrom [| Input(Parameter(0,0))*Input(Parameter(0,0))+Input(Parameter(1,0))*Input(Parameter(0,1)); 
                                                 Input(Parameter(0,1))*Input(Parameter(0,0))+Input(Parameter(1,1))*Input(Parameter(0,1));
                                                 Input(Parameter(0,2))*Input(Parameter(0,0))+Input(Parameter(1,2))*Input(Parameter(0,1))|]
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``DotProduct of two Matrices`` () = 
        let m1 = Node.Matrix.init Parameter (2,3)
        let m2 = Node.Matrix.init Parameter (3,2)
        let actual = Node.Matrix.matrixProduct m1 m2
        let a11 = Input(Parameter(0,0))*Input(Parameter(0,0))+Input(Parameter(0,1))*Input(Parameter(1,0))+Input(Parameter(0,2))*Input(Parameter(2,0))
        let a12 = Input(Parameter(1,0))*Input(Parameter(0,0))+Input(Parameter(1,1))*Input(Parameter(1,0))+Input(Parameter(1,2))*Input(Parameter(2,0))
        let a21 = Input(Parameter(0,0))*Input(Parameter(0,1))+Input(Parameter(0,1))*Input(Parameter(1,1))+Input(Parameter(0,2))*Input(Parameter(2,1))
        let a22 = Input(Parameter(1,0))*Input(Parameter(0,1))+Input(Parameter(1,1))*Input(Parameter(1,1))+Input(Parameter(1,2))*Input(Parameter(2,1))
        let expected = Node.Matrix.create (2,2) [| Node.Vector.createFrom([|a11;a21|]); Node.Vector.createFrom([|a12;a22|]) |] 
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Univariate Linear Combinaison`` () = 
        let actual = Node.linearCombinaison 0 1 2
        let expected = Input(Parameter(0,1))*Input(Variable(0,1))+Input(Parameter(0,2))*Input(Variable(0,2))
        Assert.Equal<Graph>(expected, actual)
    
    [<Fact>]
    let ``Multivariate Linear Combinaison`` () = 
        let actual = Node.multivariateLinearCombinaison 1 2 1
        let expected = [|Input(Parameter(0,0))*Input(Variable(0,1))+Input(Parameter(0,1))*Input(Variable(0,2))|]
        Assert.Equal<Graph[]>(expected, actual)
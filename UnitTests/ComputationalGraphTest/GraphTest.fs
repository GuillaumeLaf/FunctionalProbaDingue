module GraphTest
    open Xunit
    open CG.Graph
    open FSharpPlus.Data
    open FSharpPlus

    [<Fact>]
    let ``run graph and treat indices as data`` () = 
        let g : Graph<int> = Input(Parameter(0)) + Input(Parameter(1)) + Input(Parameter(2)) * Input(Parameter(3))
        let initialState = S([|0;1;2;3|], [|0|], [|0|])
        let result = Graph<int>.run g
        let expected = 7
        Assert.Equal<int>(expected, result)

    [<Fact>]
    let ``Create State monad with graph and runs it`` () = 
        let (g:Graph<int>) = Input(Parameter(0)) + Input(Parameter(1)) + Input(Parameter(2)) * Input(Parameter(3))
        let initialState : S<int> = S([|0;1;2;3|], [|0|], [|0|])
        let m = Graph<int>.ToMonad g
        let expected = 7
        let actual = State.eval m initialState
        Assert.Equal<int>(expected, actual)

    [<Fact>]
    let ``Humongous graph to check StackOverflow`` () = 
        let g = Seq.init 1000000 (fun _ -> Input(Parameter(0))) |> sum
        let initialState = S([|0|], [|0|], [|0|])
        let m = Graph<int>.ToMonad g
        let expected = 0
        let actual = State.eval m initialState
        Assert.Equal<int>(expected, actual)


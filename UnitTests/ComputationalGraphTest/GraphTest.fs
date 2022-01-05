module GraphTest
   open Xunit
   open CG.Graph
   open FSharpPlus.Data
   open FSharpPlus
   
   let (g:Graph<int>) = Input(Parameter(0)) + Input(Parameter(1)) * Input(Parameter(2))
   let initialState = S([|0;1;2|], [|0|], [|0|])

   [<Fact>]
   let ``run graph and treat indices as data`` () = 
       let result = Graph.run g
       let expected = 2
       Assert.Equal<int>(expected, result)

   [<Fact>]
   let ``Create State monad with graph and runs it`` () = 
        let m = Graph.ToMonad g
        let expected = 2
        let actual = State.eval m initialState
        Assert.Equal<int>(expected, actual)
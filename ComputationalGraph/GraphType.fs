namespace ComputationalGraph

open System.Runtime.CompilerServices

module GraphType = 
    
    [<IsReadOnly;Struct>]
    type BasicInput = 
        | Parameter of GroupidxP:int * Parameteridx:int
        | Variable of GroupidxV:int * Variableidx:int
        | Innovation of GroupidxI:int * Innovaitonidx:int

    // Monadic graph type allows easy manipulation and update of graph.
    // The Monad State must be in agreement with the number of 'Input' nodes in the graph.
    // Fst : 'n'th timeseries
    // Snd : number of paramters for the 'n'th timeseries
    // Note : there is a unique innovation for each unique timeseries model.
    type S = S of parameters:float32[,] * variables:float32[,] * innovations:float32[,]

    // The most important part of this project.
    // Creating a computational graph eases creation of complex computations. 
    // The ideal would be to begin with a classical computational graph and then convert/compile it to blazingly fast arrays operations. 
    type Graph = 
        | Input of BasicInput
        | Constant of float32
        | Polynomial of Graph * int
        | Addition of Graph * Graph
        | Substraction of Graph * Graph
        | Multiplication of Graph * Graph

        static member ( + ) (g1:Graph, g2:Graph) = Addition(g1,g2)
        static member ( - ) (g1:Graph, g2:Graph) = Substraction(g1,g2)
        static member ( * ) (g1:Graph, g2:Graph) = Multiplication(g1,g2)
        static member Pow (g:Graph, e:int) = Polynomial(g,e)





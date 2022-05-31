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
    // They could be missing.
    type S< 'T > = S of parameters: 'T[,] * variables: 'T[,] * innovations: 'T[,]

    // The most important part of this project.
    // Creating a computational graph eases creation of complex computations. 
    // The ideal would be to begin with a classical computational graph and then convert/compile it to blazingly fast arrays operations. 
    type Graph< 'T > = 
        | Input of BasicInput
        | Constant of 'T
        | Polynomial of Graph< 'T > * int
        | Addition of Graph< 'T > * Graph< 'T >
        | Substraction of Graph< 'T > * Graph< 'T >
        | Multiplication of Graph< 'T > * Graph< 'T >

        static member inline ( + ) (g1:Graph< 'T >, g2:Graph< 'T >) = Addition(g1,g2)
        static member inline ( - ) (g1:Graph< 'T >, g2:Graph< 'T >) = Substraction(g1,g2)
        static member inline ( * ) (g1:Graph< 'T >, g2:Graph< 'T >) = Multiplication(g1,g2)
        static member inline Pow (g:Graph< 'T >, e:int) = Polynomial(g,e)





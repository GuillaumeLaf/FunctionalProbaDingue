﻿namespace ComputationalGraph

open System.Runtime.CompilerServices

module GraphType = 
    
    [<IsReadOnly;Struct>]
    type BasicInput = 
        | Parameter of GroupidxP:int * Parameteridx:int
        | Variable of GroupidxV:int * Variableidx:int
        | Innovation of GroupidxI:int * Innovaitonidx:int

    // Monadic graph type allows easy manipulation and update of graph.
    // The Monad State must be in agreement with the number of 'Input' nodes in the graph.
    // Note : there is a unique innovation for each unique timeseries model.
    type S = S of parameters:float32[,] * variables:float32[,] * innovations:float32[]

    // The most important part of this project.
    // Creating a computational graph eases creation of complex computations. 
    // The ideal would be to begin with a classical computational graph and then convert/compile it to blazingly fast arrays operations. 
    type Graph = 
        | Input of BasicInput
        | Constant of float32
        | Addition of Graph * Graph
        | Multiplication of Graph * Graph




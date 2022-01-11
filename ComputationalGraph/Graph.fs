namespace ComputationalGraph

open System.Runtime.CompilerServices
open FSharpPlus.Data
open FSharpPlus

module Graph =

    [<AutoOpen>]
    module Inputs = 
        
        // Type that represents nodes that are considered as input in the graph
        // I can do a little algebra with them and group them which will allow multivariate graph.
        [<IsReadOnly;Struct>]
        type Input = 
            | Parameter of GroupidxP:int * Parameteridx:int
            | Variable of GroupidxV:int * Variableidx:int
            static member inline op_Equality (input1:Input,input2:Input) = match (input1,input2) with
                                                                            | Parameter(grpidx1,idx1),Parameter(grpidx2,idx2) -> (grpidx1 = grpidx2) && (idx1 = idx2)  
                                                                            | Variable(grpidx1,idx1),Variable(grpidx2,idx2) -> (grpidx1 = grpidx2) && (idx1 = idx2)
                                                                            | _,_ -> false
                    
            static member inline (+) (input:Input, n:int) = match input with 
                                                            | Parameter(grpidx,idx) -> Parameter(grpidx,idx+n)
                                                            | Variable(grpidx,idx) -> Variable(grpidx,idx+n)

            static member inline (+) (n:int, input:Input) = (+) input n

    module Monad =     
        // Monadic graph type allows easy manipulation and update of graph.
        // The Monad State must be in agreement with the number of 'Input' nodes in the graph.
        type S<'T> = S of parameters:'T[][] * variables:'T[][] * innovations:'T[][]

        // £xtract the whole array of the 'S' type.
        let getParameters (S(p,_,_)) = p
        let getVariables (S(_,v,_)) = v
        let getInnovations (S(_,_,i)) = i

        // Convert the extracted 'S' arrays into monad to be further composed.
        let parametersM () = State.gets getParameters
        let variablesM () = State.gets getVariables
        let innovationsM () = State.gets getInnovations
        
        // Get the given group of 'Input' of the 'S'.
        let parameterForGroupM grpidx = Array.get <!> parametersM() <*> result grpidx
        let variableForGroupM grpidx = Array.get <!> variablesM() <*> result grpidx
        let innovationForGroupM grpidx = Array.get <!> innovationsM() <*> result grpidx

        // Get the individual 'Input' data in 'S'.
        let parameterM grpidx idx = Array.get <!> parameterForGroupM grpidx <*> result idx
        let variableM grpidx idx = Array.get <!> variableForGroupM grpidx <*> result idx
        let innovationM grpidx idx = Array.get <!> innovationForGroupM grpidx <*> result idx

    // The most important part of this project.
    // Creating a computational graph eases creation of complex computations. 
    // The ideal would be to begin with a classical computational graph and then convert/compile it to blazingly fast arrays operations. 
    type Graph<'T> = 
        | DataPoint of Input
        | Constant of value:'T
        | Addition of Graph<'T> * Graph<'T>
        | Multiplication of Graph<'T> * Graph<'T>
        static member inline get_Zero() = Constant(LanguagePrimitives.GenericZero)
        static member inline ( + ) (l:Graph<'T>, r:Graph<'T>) = Addition(l,r)
        static member inline ( * ) (l:Graph<'T>, r:Graph<'T>) = Multiplication(l,r)

        // Concatenate in an array all the 'Input' nodes of the graph 'x'. 
        static member inline collectInputs (x:Graph<'T>) = 
            let rec loop g acc = monad {
                match g with
                | Constant(_) -> return acc
                | Addition(l,r) | Multiplication(l,r) -> let! lacc = (loop l acc)
                                                         let! racc = (loop r acc)
                                                         return lacc @ racc
                | DataPoint(i) ->  return i::acc 
            }
            Cont.run (loop x []) id |> Array.ofList
        
        // Get the 'Input's contained in the graph as an array.
        // There is only one 'Input' type -e.g. Parameter(_,_), Variable(_,_)- for each group of 'Input'.
        // For instance if the output array is of the form : [|Parameter(0,2);Variable(1,3)|], 
        // It translates into a graph with 2 'Parameter's in group 0,
        // and 3 'Variable's in group 1.
        static member inline groupSizes (x:Graph<'T>) = 
            Graph.collectInputs x |> Array.countBy (function | Parameter(grpidx,_) -> Parameter(grpidx,0)
                                                             | Variable(grpidx,_) -> Variable(grpidx,0))
                                  |> Array.map (function | Parameter(grpidx,_),c -> Parameter(grpidx,c)
                                                         | Variable(grpidx,_),c -> Variable(grpidx,c))
                                  |> Array.sortBy (function | Parameter(grpidx,_) | Variable(grpidx,_) -> grpidx)

        // Run the graph with the indices of the Inputs as data.
        // This method was primarily to test the graph's computations.
        static member inline run (x:Graph<int>) = 
            let opCont op lg rg f : Cont<'a,'b> = monad { let! xl = f lg
                                                          let! xr = f rg
                                                          return op xl xr }
            let rec loop g = monad {
                match g with
                | Constant(value) -> return value
                | Addition(lg,rg) -> return! opCont ( + ) lg rg loop
                | Multiplication(lg,rg) -> return! opCont ( * ) lg rg loop
                | DataPoint(i) -> match i with
                                  | Parameter(_,idx) -> return idx
                                  | Variable(_,idx) -> return idx
            }
            Cont.run (loop x) id

        // Create a State Monad from the Graph
        // Working with monads makes composing computations easier and,
        // allows one to implement abstract ideas easily without caring to much about the details.
        // However, for speedy computations Monads may not be the best option.
        // In the far end, one should create a compiler with faster than light compiled Monad operations.
        // But for now, I trust 'FSharpPlus' to efficiently compile my Monads.
        static member inline ToMonad (x:Graph<'a>) = 
            let rec loop g : ContT<State<Monad.S<'a>,'b>,'a> = monad {
                match g with
                | Constant(value) -> return! lift (result value)
                | Addition(l,r) -> return! (+) <!> (loop l) <*> (loop r)
                | Multiplication(l,r) -> return! (*) <!> (loop l) <*> (loop r)
                | DataPoint(i) -> match i with
                                  | Parameter(grpidx,idx) -> return! lift (Monad.parameterM grpidx idx)
                                  | Variable(grpidx,idx) -> return! lift (Monad.variableM grpidx idx)
            }
            ContT.eval (loop x)




        
namespace ComputationalGraph

open System.Runtime.CompilerServices
open FSharpPlus.Data
open FSharpPlus

module Graph =

    [<AutoOpen>]
    module Inputs = 

        [<IsReadOnly;Struct>]
        type Input = 
            | Parameter of Pidx:int
            | Variable of Vidx:int
            static member inline op_Equality (input1:Input, input2:Input) = match (input1,input2) with
                                                                                    | Parameter(idx1),Parameter(idx2) -> idx1 = idx2
                                                                                    | Variable(idx1),Variable(idx2) -> idx1 = idx2
                                                                                    | _ -> false
            static member inline (+) (input:Input, n:int) = match input with 
                                                                  | Parameter(idx) -> Parameter(idx+n)
                                                                  | Variable(idx) -> Variable(idx+n)
            static member inline (+) (n:int, input:Input) = match input with 
                                                                  | Parameter(idx) -> Parameter(idx+n)
                                                                  | Variable(idx) -> Variable(idx+n)

    module Monad =                                                            
        type S<'T> = S of parameters:'T[] * variables:'T[] * innovations:'T[]

        let getParameters (S(p,_,_)) = p
        let getVariables (S(_,v,_)) = v
        let getInnovations (S(_,_,i)) = i

        let parametersM () = State.gets getParameters
        let variablesM () = State.gets getVariables
        let innovationsM () = State.gets getInnovations

        let parameterM idx = Array.get <!> parametersM() <*> result idx
        let variableM idx = Array.get <!> variablesM() <*> result idx
        let innovationM idx = Array.get <!> innovationsM() <*> result idx


    type Graph<'T> = 
        | DataPoint of Input
        | Constant of value:'T
        | Addition of Graph<'T> * Graph<'T>
        | Multiplication of Graph<'T> * Graph<'T>
        static member inline get_Zero() = Constant(LanguagePrimitives.GenericZero)
        static member inline (+) (l:Graph<'T>, r:Graph<'T>) = Addition(l,r)
        static member inline (*) (l:Graph<'T>, r:Graph<'T>) = Multiplication(l,r)
        
        // Get the number of input nodes in the graph. 
        // The output consists in an array of Input where the index represent its count.
        static member inline size (x:Graph<'T>) = 
            let count = [|Parameter(0); Variable(0)|]
            let rec loop g = monad {
                match g with
                | Constant(_) -> return ()
                | Addition(l,r) | Multiplication(l,r) -> do! (loop l)
                                                         do! (loop r)
                | DataPoint(i) -> match i with
                                    | Parameter(_) -> return (count.[0] <- count.[0] + 1)
                                    | Variable(_) -> return (count.[1] <- count.[1] + 1)
            }
            Cont.run (loop x) id |> ignore
            count

        // Run the graph with the indices of the Inputs as data
        static member inline run (x:Graph<int>) = 
            let opCont op lg rg f : Cont<'a,'b> = monad { let! xl = f lg
                                                          let! xr = f rg
                                                          return op xl xr }
            let rec loop g = monad {
                match g with
                | Constant(value) -> return value
                | Addition(lg,rg) -> return! opCont (+) lg rg loop
                | Multiplication(lg,rg) -> return! opCont (*) lg rg loop
                | DataPoint(i) -> match i with
                                  | Parameter(idx) -> return idx
                                  | Variable(idx) -> return idx
            }
            Cont.run (loop x) id

        // Create a State monad from the Graph
        static member inline ToMonad (x:Graph<'a>) = 
            let rec loop g : ContT<State<Monad.S<'a>,'b>,'a> = monad {
                match g with
                | Constant(value) -> return! lift (result value)
                | Addition(l,r) -> return! (+) <!> (loop l) <*> (loop r)
                | Multiplication(l,r) -> return! (*) <!> (loop l) <*> (loop r)
                | DataPoint(i) -> match i with
                                  | Parameter(idx) -> return! lift (Monad.parameterM idx)
                                  | Variable(idx) -> return! lift (Monad.variableM idx)
            }
            ContT.eval (loop x)




        
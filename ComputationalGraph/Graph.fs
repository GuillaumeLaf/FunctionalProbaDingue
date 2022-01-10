namespace ComputationalGraph

open System.Runtime.CompilerServices
open FSharpPlus.Data
open FSharpPlus

module Graph =

    [<AutoOpen>]
    module Inputs = 

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
        type S<'T> = S of parameters:'T[][] * variables:'T[][] * innovations:'T[][]

        let getParameters (S(p,_,_)) = p
        let getVariables (S(_,v,_)) = v
        let getInnovations (S(_,_,i)) = i

        let parametersM () = State.gets getParameters
        let variablesM () = State.gets getVariables
        let innovationsM () = State.gets getInnovations
        
        let parameterForGroupM grpidx = Array.get <!> parametersM() <*> result grpidx
        let variableForGroupM grpidx = Array.get <!> variablesM() <*> result grpidx
        let innovationForGroupM grpidx = Array.get <!> innovationsM() <*> result grpidx

        let parameterM grpidx idx = Array.get <!> parameterForGroupM grpidx <*> result idx
        let variableM grpidx idx = Array.get <!> variableForGroupM grpidx <*> result idx
        let innovationM grpidx idx = Array.get <!> innovationForGroupM grpidx <*> result idx

    type Graph<'T> = 
        | DataPoint of Input
        | Constant of value:'T
        | Addition of Graph<'T> * Graph<'T>
        | Multiplication of Graph<'T> * Graph<'T>
        static member inline get_Zero() = Constant(LanguagePrimitives.GenericZero)
        static member inline (+) (l:Graph<'T>, r:Graph<'T>) = Addition(l,r)
        static member inline (*) (l:Graph<'T>, r:Graph<'T>) = Multiplication(l,r)

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
        
        // Get the number of input nodes in the graph. 
        // The output consists in an array of Input where the index represent its count.
        static member inline groupSizes (x:Graph<'T>) = 
            Graph.collectInputs x |> Array.groupBy (function | Parameter(grpidx,_) | Variable(grpidx,_) -> grpidx)
                                  |> Array.map (fun (_,inputs) -> inputs |> Array.countBy (function | Parameter(grpidx,_) -> Parameter(grpidx,0)
                                                                                                    | Variable(grpidx,_) -> Variable(grpidx,0)))
                                  |> Array.map (fun arr -> arr |> Array.map (function | Parameter(grpidx,_),c -> Parameter(grpidx,c)
                                                                                      | Variable(grpidx,_),c -> Variable(grpidx,c)))

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
                                  | Parameter(_,idx) -> return idx
                                  | Variable(_,idx) -> return idx
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
                                  | Parameter(grpidx,idx) -> return! lift (Monad.parameterM grpidx idx)
                                  | Variable(grpidx,idx) -> return! lift (Monad.variableM grpidx idx)
            }
            ContT.eval (loop x)




        
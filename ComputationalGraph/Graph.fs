namespace CG

open FSharpPlus.Data
open FSharpPlus

module Graph = 

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
    
    type Input<'T> = 
        | Parameter of idx:int
        //| Variable of idx:int

    type Graph<'T> = 
        | Input of Input<'T>
        | Addition of Graph<'T> * Graph<'T>
        | Multiplication of Graph<'T> * Graph<'T>
        static member inline (+) (l:Graph<'T>, r:Graph<'T>) = Addition(l,r)
        static member inline (*) (l:Graph<'T>, r:Graph<'T>) = Multiplication(l,r)
        
        // Run the graph with the indices of the Inputs as data
        static member inline run (x:Graph<'T>) = 
            let opCont op lg rg f : Cont<'a,'b> = monad { let! xl = f lg
                                                          let! xr = f rg
                                                          return op xl xr }
            let rec loop g = monad {
                match g with
                | Addition(lg,rg) -> return! opCont (+) lg rg loop
                | Multiplication(lg,rg) -> return! opCont (*) lg rg loop
                | Input(i) -> match i with
                              | Parameter(idx) -> return idx
            }
            Cont.run (loop x) id

        // Create a State monad from the Graph
        static member inline ToMonad (x:Graph<'T>) = 
            let opCont op lg rg f : Cont<'a,State<'b,'c>> = monad { let! xl = f lg
                                                                    let! xr = f rg
                                                                    return op <!> xl <*> xr }
            let rec loop g = monad {
                match g with 
                | Addition(l,r) -> return! opCont (+) l r loop
                | Multiplication(l,r) -> return! opCont (*) l r loop
                | Input(i) -> match i with
                              | Parameter(idx) -> return parameterM idx
            }
            Cont.run (loop x) id

        
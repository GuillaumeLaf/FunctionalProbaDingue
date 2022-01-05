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
        | Constant of value:'T

    type Graph<'T> = 
        | Input of Input<'T>
        | Addition of Graph<'T> * Graph<'T>
        | Multiplication of Graph<'T> * Graph<'T>
        static member inline get_Zero() = Input(Constant(LanguagePrimitives.GenericZero)) // ONLY INT !
        static member inline (+) (l:Graph<'T>, r:Graph<'T>) = Addition(l,r)
        static member inline (*) (l:Graph<'T>, r:Graph<'T>) = Multiplication(l,r)
        
        // Run the graph with the indices of the Inputs as data
        static member inline run (x:Graph<int>) = 
            let opCont op lg rg f : Cont<'a,'b> = monad { let! xl = f lg
                                                          let! xr = f rg
                                                          return op xl xr }
            let rec loop g = monad {
                match g with
                | Addition(lg,rg) -> return! opCont (+) lg rg loop
                | Multiplication(lg,rg) -> return! opCont (*) lg rg loop
                | Input(i) -> match i with
                              | Parameter(idx) -> return idx
                              | Constant(value) -> return value
            }
            Cont.run (loop x) id

        // Create a State monad from the Graph
        static member inline ToMonad (x:Graph<'a>) = 
            let rec loop g : ContT<State<S<'a>,'b>,'a> = monad {
                match g with
                | Addition(l,r) -> return! (+) <!> (loop l) <*> (loop r)
                | Multiplication(l,r) -> return! (*) <!> (loop l) <*> (loop r)
                | Input(i) -> match i with
                              | Parameter(idx) -> return! lift (parameterM idx)
                              | Constant(value) -> return! lift (result value)
            }
            ContT.eval (loop x)


        
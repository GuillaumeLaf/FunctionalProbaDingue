namespace ComputationalGraph

open FSharpPlus
open FSharpPlus.Data
open GraphType

module GraphState = 
    
    // Extract the whole array of the 'S' type.
    let inline getParameters (S(p,_,_)) = p
    let inline getVariables (S(_,v,_)) = v
    let inline getInnovations (S(_,_,i)) = i

    // Convert the extracted 'S' arrays into monad to be further composed.
    let inline parametersM () = State.gets getParameters              : State<S< ^T >, ^T[,]>
    let inline variablesM () = State.gets getVariables                : State<S< ^T >, ^T[,]>
    let inline innovationsM () = State.gets getInnovations            : State<S< ^T >, ^T[,]>

    // Get the individual 'Input' data in 'S'.
    // Fst : which Group/timeseries
    // Snd : which parameter
    let inline parameterM grpidx idx = Array2D.get <!> parametersM() <*> result grpidx <*> result idx
    let inline variableM grpidx idx = Array2D.get <!> variablesM() <*> result grpidx <*> result idx
    let inline innovationM grpidx idx = Array2D.get <!> innovationsM() <*> result grpidx <*> result idx

    // Update 'S' in the State of the monad.
    let inline updateParameters newParameters = State.modify (fun (S(_,v,i)) -> S(newParameters,v,i))
    let inline updateVariables newVariables = State.modify (fun (S(p,_,i)) -> S(p,newVariables,i))
    let inline updateInnovations newInnovations = State.modify (fun (S(p,v,_)) -> S(p,v,newInnovations))


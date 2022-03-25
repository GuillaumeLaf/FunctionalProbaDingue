namespace ComputationalGraph

open FSharpPlus
open FSharpPlus.Data
open GraphType

module GraphState = 
    
    // Extract the whole array of the 'S' type.
    let getParameters (S(p,_,_)) = p
    let getVariables (S(_,v,_)) = v
    let getInnovations (S(_,_,i)) = i

    // Convert the extracted 'S' arrays into monad to be further composed.
    let parametersM = State.gets getParameters              : State<S,float32 option[,]>
    let variablesM = State.gets getVariables                : State<S,float32 option[,]>
    let innovationsM = State.gets getInnovations            : State<S,float32 option[,]>

    // Get the individual 'Input' data in 'S'.
    // Fst : which Group/timeseries
    // Snd : which parameter
    let parameterM grpidx idx = Array2D.get <!> parametersM <*> result grpidx <*> result idx
    let variableM grpidx idx = Array2D.get <!> variablesM <*> result grpidx <*> result idx
    let innovationM grpidx idx = Array2D.get <!> innovationsM <*> result grpidx <*> result idx

    // Update 'S' in the State of the monad.
    let updateParameters newParameters = State.modify (fun (S(_,v,i)) -> S(newParameters,v,i))
    let updateVariables newVariables = State.modify (fun (S(p,_,i)) -> S(p,newVariables,i))
    let updateInnovations newInnovations = State.modify (fun (S(p,v,_)) -> S(p,v,newInnovations))


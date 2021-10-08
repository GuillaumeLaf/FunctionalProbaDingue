namespace Models

open Monads

module SGD = 

    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x

    let limitParams x =
        match x with
        | a when x < -1.0 -> -1.0
        | a when x > 1.0 -> 1.0
        | _ -> x
    
    let updateRuleM learningRate skeleton parameterIdx parameterValue currentError = 
        Monad.state {
            let! gradient = GraphTS.runGraphM (Graph.skeletonGradientForParameterM parameterIdx skeleton)
            return parameterValue - learningRate * gradient * currentError
        }

    // Updating of the parameters must be made after running the model.
    let updateParametersM learningRate skeleton indices = 
        Monad.state {
            let! currentError = GraphTS.runTimeSeriesM (TimeSeries.Univariate.currentInnovationM ())
            let currentError = currentError |> Option.defaultValue 0.0
            let! parameterValues = GraphTS.runGraphM Graph.parametersM
            let! newParameters = Array.mapi (fun i value -> updateRuleM learningRate skeleton i value currentError) parameterValues |> Monad.mapM
            let newParameters = newParameters |> Array.map (fun x -> limitParams x)
            do! GraphTS.runGraphM (Graph.setParametersM newParameters)
        }
            
    let fit model learningRate epochs array = 
        let model = (ErrorModel(model))
        let initStateTS = TimeSeries.Univariate.defaultStateFrom array
        let initStateG = Graph.defaultStateForFitting model
        let defaultSk = Graph.defaultSkeletonForFitting model
        let errorSkM = model |> Fitting |> Graph.modelM
        let updateVarM = GraphTS.updateForFittingM model

        let fittingM indices = 
            Monad.state {
                do! GraphTS.SDGfitM updateVarM errorSkM indices
                do! updateParametersM learningRate defaultSk indices
            }
                                  
        let folder states = Array.fold (fun s idx -> let _,nxtStates = s |> Monad.run (fittingM idx) 
                                                     nxtStates) states                              
        
        let printCurrent states = 
            let (Graph.State(p,_,_),TimeSeries.Univariate.State(_,_,errors)) = states
            let errors = errors |> Array.map (fun x -> x |> Option.defaultValue 0.0)
            errors |> Array.mapFold (fun s x -> (),x*x+s) 0.0 |> snd, p

        let rec loop idxArray epochs states = 
            match epochs with
            | 1 -> printfn "%A" (printCurrent states); folder states (idxArray |> Array.chunkBySize 10)
            | _ -> printfn "%A" (printCurrent states); loop (Utilities.shuffle idxArray) (epochs-1) (folder states (idxArray |> Array.chunkBySize 10))
        loop (Array.init array.Length id) epochs (initStateG,initStateTS) |> fst

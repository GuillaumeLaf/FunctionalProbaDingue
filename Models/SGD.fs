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
            return parameterValue + learningRate * gradient * currentError
        }

    let updateParametersM learningRate skeleton = 
        Monad.state {
            let! currentError = GraphTS.runTimeSeriesM (TimeSeries.Univariate.currentInnovationM ())
            let currentError = currentError |> Option.defaultValue 0.0
            let! parameterValues = GraphTS.runGraphM Graph.parametersM
            let! newParameters = Array.mapi (fun i value -> updateRuleM learningRate skeleton i value currentError) parameterValues |> Monad.mapM
            let newParameters = newParameters |> Array.map (fun x -> limitParams x)
            do! GraphTS.runGraphM (Graph.setParametersM newParameters)
        }

    let lossForEpochM model array = 
        Monad.state {
            let! errors = GraphTS.runGraphM (GraphTS.getError model array <!> Monad.get)
            return (Array.mapFoldBack (fun x s -> (), x*x+s) errors 0.0 |> snd)
        }
            
    let fit model learningRate epochs array = 
        let initStateTS = TimeSeries.Univariate.defaultStateFrom array
        let initStateG = Graph.defaultStateForFitting model
        let defaultSk = Graph.defaultSkeletonForFitting model
        let skM = Graph.modelM (Fitting(model))
        let updateVarM = GraphTS.updateForFittingM model

        let fittingM idx = 
            Monad.state {
                do! GraphTS.SDGfitM updateVarM skM idx
                do! updateParametersM learningRate defaultSk
            }
                                  
        let folder states = Array.fold (fun s idx -> let _,nxtStates = s |> Monad.run (fittingM idx) 
                                                     nxtStates) states                              
        
        let printCurrent states= 
            let r,(s,_) = Monad.run (lossForEpochM model array) states
            r,s

        let rec loop idxArray epochs states = 
            match epochs with
            | 1 -> printfn "%A" (printCurrent states); folder states idxArray
            | _ -> printfn "%A" (printCurrent states); loop (Utilities.shuffle idxArray) (epochs-1) (folder states idxArray)
        loop (Array.init array.Length id) epochs (initStateG,initStateTS) |> fst

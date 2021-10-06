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
    
    let updateRuleM learningRate skeleton parameterIdx parameterValue = 
        Monad.state {
            let! gradient = Graph.skeletonGradientForParameterM parameterIdx skeleton
            return parameterValue + learningRate * gradient
        }

    let updateParametersM learningRate skeleton = 
        Monad.state {
            let! parameterValues = Graph.parametersM
            let! newParameters = Array.mapi (fun i value -> updateRuleM learningRate skeleton i value) parameterValues |> Monad.mapM
            let newParameters = newParameters |> Array.map (fun x -> limitParams x)
            do! Graph.setParametersM newParameters
        }

    let lossForEpoch model array =
        GraphTS.getError model array <!> Monad.get
            >>= (fun stateTS -> let (TimeSeries.Univariate.State(_,_,innov)) = stateTS
                                Array.fold (fun s xOption -> Option.fold (fun accum x -> accum + x*x) s xOption) 0.0 innov |> Monad.rets)
            
    let fit model learningRate epochs array = 
        let initStateTS = TimeSeries.Univariate.defaultStateFrom array
        let initStateG = Graph.defaultStateForFitting model
        let defaultSk = Graph.defaultSkeletonForFitting model
        let skM = Graph.modelM (Fitting(model))
        let updateVarM = GraphTS.updateForFittingM model

        let fittingM idx =
            (GraphTS.SDGfitM updateVarM skM idx) 
                |> BiMonad.bind (fun _ _ -> BiMonad.modifySecondWithMonad (updateParametersM learningRate defaultSk))
                                  
        let folder states = Array.fold (fun s idx -> let _,_,nxtS1,nxtS2 = s ||> BiMonad.run (fittingM idx) 
                                                     (nxtS1,nxtS2)) states                              
        
        let rec loop idxArray epochs states = 
            let (_,s2) = states
            match epochs with
            | 1 -> printfn "%A" (Monad.run (lossForEpoch model array) s2); folder states idxArray
            | _ -> printfn "%A" (Monad.run (lossForEpoch model array) s2); loop (Utilities.shuffle idxArray) (epochs-1) (folder states idxArray)
        loop (Array.init array.Length id) epochs (initStateTS,initStateG) 

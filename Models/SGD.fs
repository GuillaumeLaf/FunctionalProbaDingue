namespace Models

open Monads

module SGD = 

    let (<*>) = Monad.apply
    let (<!>) = Monad.map
    let (>>=) x f = Monad.bind f x
    let (>>==) x f = BiMonad.bind f x
    let (>>=>>) g f = BiMonad.compose g f

    let SDGfitM updateM skM idx = 
        GraphTimeSeries.setCurrentIndexM () idx >>== (fun _ _ -> updateM >>== (fun _ _ -> (GraphTimeSeries.activateModelM >>=>> GraphTimeSeries.getCurrentErrorM) () skM))

    let limitParams x =
        match x with
        | a when x < -1.0 -> -1.0
        | a when x > 1.0 -> 1.0
        | _ -> x

    let updateParametersM learningRate skeleton = 
        (Array.mapi (fun i x -> Monad.add (Monad.rets x) 
                                 (Monad.mult (Monad.rets learningRate) 
                                   (Graph.skeletonGradientForParameterM i skeleton ))) 
                <!> Graph.parametersM 
                >>= (fun arrayM -> Monad.mapM arrayM))
                >>= (fun array -> Graph.setParametersM (array |> Array.map (fun x -> limitParams x)))
    
    let lossForEpoch model array =
        GraphTimeSeries.getError model array <!> Graph.stateM
            >>= (fun stateTS -> let (TimeSeries.Univariate.State(_,_,innov)) = stateTS
                                Array.fold (fun s xOption -> Option.fold (fun accum x -> accum + x*x) s xOption) 0.0 innov |> Monad.rets)
            
    let fit model learningRate epochs array = 
        let initStateTS = TimeSeries.Univariate.defaultStateFrom array
        let initStateG = Graph.defaultStateForFitting model
        let defaultSk = Graph.defaultSkeletonForFitting model
        let skM = Graph.modelM (Fitting(model))
        let updateVarM = GraphTimeSeries.updateVariablesForFittingM model

        let fittingM idx =
            (SDGfitM updateVarM skM idx) 
                |> BiMonad.bind (fun _ _ -> BiMonad.modifySecondWithMonad (updateParametersM learningRate defaultSk))
                                  
        let folder states = Array.fold (fun s idx -> let _,_,nxtS1,nxtS2 = s ||> BiMonad.run (fittingM idx) 
                                                     (nxtS1,nxtS2)) states                              
        
        let rec loop idxArray epochs states = 
            let (_,s2) = states
            match epochs with
            | 1 -> printfn "%A" (Monad.run (lossForEpoch model array) s2); folder states idxArray
            | _ -> printfn "%A" (Monad.run (lossForEpoch model array) s2); loop (Utilities.shuffle idxArray) (epochs-1) (folder states idxArray)
        loop (Array.init array.Length id) epochs (initStateTS,initStateG) 

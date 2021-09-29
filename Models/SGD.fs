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

    let updateParametersM learningRate skeleton = 
        (Array.mapi (fun i x -> Monad.sub (Monad.rets x) 
                                 (Monad.mult (Monad.rets learningRate) 
                                   (MonadicGraph.skeletonGradientForParameterM i skeleton)))
                <!> MonadicGraph.parametersM 
                >>= (fun arrayM -> MonadicGraph.mapM arrayM))
                >>= (fun array -> MonadicGraph.setParametersM (array |> Array.map (fun x -> limitParams x)))
            
    let fit model learningRate epochs array = 
        let initStateTS = TimeSeries.Univariate.defaultStateFrom array
        let initStateG = MonadicGraph.defaultStateForFitting model
        let defaultSk = MonadicGraph.defaultSkeletonForFitting model
        let skM = MonadicGraph.modelM (Fitting(model))
        let updateVarM = GraphTimeSeries.updateVariablesForFittingM model

        let fittingM idx =
            BiMonad.bind (fun _ _ -> BiMonad.modifySecondWithMonad (updateParametersM learningRate defaultSk))
                         (GraphTimeSeries.SDGfitM updateVarM skM idx)         

        let folder states = Array.fold (fun s idx -> let _,_,nxtS1,nxtS2 = s ||> BiMonad.run (fittingM idx) 
                                                     (nxtS1,nxtS2)) states                              
        
        let rec loop array epochs states = 
            match epochs with
            | 1 -> folder states array
            | _ -> loop (Utilities.shuffle array) (epochs-1) (folder states array)
        loop (Array.init array.Length id) epochs (initStateTS,initStateG) 

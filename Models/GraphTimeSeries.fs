namespace Models

open Monads

module GraphTimeSeries = 

    let (>>=) x f = BiMonad.bind f x
    let (<!>) = BiMonad.map
    let (<*>) = BiMonad.apply
    let (>=>) g f = Monad.compose g f
    let (>>=>>) g f = BiMonad.compose g f

    let defaultState name n = (TimeSeries.UnivariateTimeSeries.defaultState n,MonadicGraph.defaultState name)

    let _activateModelM () skM = 
        let innerFunc (stateTS:TimeSeries.UnivariateTimeSeries.State<'T>) (stateG:MonadicGraph.State<'T>) = 
            let result, nxtStateG = Monad.run skM stateG
            (), result, stateTS, nxtStateG
        BiMonad.M innerFunc
    
    // Variable update must be made at a specific time ! (Be careful of look-ahead bias).
    let rec _defineUpdatesTSM modelName = 
        match modelName with
        | ARp(order) -> [ for i in 1..order.Length do TimeSeries.UnivariateTimeSeries.elementAtLagM i ]
        | MAp(order) -> [ for i in 1..order.Length do TimeSeries.UnivariateTimeSeries.innovationAtLagM i ] 
        // | SETAR(order,delay) -> Array.concat [|variableUpdate (AR(order)); [|TimeSeries.UnivariateTimeSeries.elementAtLagM delay|]|] |> Array.toList |> Monad.sequence
        |> Monad.sequence
        |> Monad.map (Array.ofList)
        |> Monad.map (Array.map (fun x -> x |> Option.defaultValue 0.0))
        
    let _updateM updateSequenceTSM = // have to update the graph state via the TimeSeries Monad.
        BiMonad.modify (fun s1 (MonadicGraph.State(p,_,i)) -> let newVariables, _ = Monad.run updateSequenceTSM s1
                                                              s1, (MonadicGraph.State(p,newVariables,i)))

    let updateVariablesForSamplingM = _defineUpdatesTSM >> _updateM
    let updateVariablesForFittingM = MonadicGraph.convertModelToParameters >> updateVariablesForSamplingM
    
    let _setCurrentInnovationM () () = // must be set before updating variables.
        BiMonad.modify (fun s1 (MonadicGraph.State(p,v,i)) -> let _, nxS1 = Monad.run (TimeSeries.UnivariateTimeSeries.setCurrentInnovationM i.[0]) s1
                                                              nxS1, (MonadicGraph.State(p,v,i)))

    let _setCurrentElementM () x = BiMonad.modifyFirstWithMonad (TimeSeries.UnivariateTimeSeries.setCurrentElementM x)
    let _stepM () () = BiMonad.modifyFirstWithMonad (TimeSeries.UnivariateTimeSeries.stepM) 

    let sampleOnceM updateM skM = 
        (_activateModelM >>=>> _setCurrentElementM >>=>> _setCurrentInnovationM >>=>> _stepM >>=>> (fun _ _ -> updateM)) () skM

    let fitOnceM errorSkM = 
        (_activateModelM >>=>>)

    let foldRun m (TimeSeries.UnivariateTimeSeries.State(idx,data,innov)) initStateG =
        data |> Array.fold (fun (s1,s2) x -> let _,_,nxS1,nxS2 = BiMonad.run m s1 s2
                                             (nxS1,nxS2)) 
                           ((TimeSeries.UnivariateTimeSeries.State(idx,data,innov)),initStateG)

    let sample n = function
        | Sampling(mparameters) -> let initStateTS = TimeSeries.UnivariateTimeSeries.defaultState n

                                   let initStateG = MonadicGraph.defaultStateForSampling mparameters
                                   let skM = MonadicGraph.modelM (Sampling(mparameters))
                                   let updteVarM = updateVariablesForSamplingM mparameters
                                   let samplingM = sampleOnceM updteVarM skM
                                   foldRun samplingM initStateTS initStateG |> fst
        | Fitting(m) -> invalidArg "model" "Cannot sample with a Fitting model type. Convert it to a Sampling type."
        
            
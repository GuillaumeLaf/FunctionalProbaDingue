namespace Models

open Monads

module GraphTimeSeries = 

    let (>>=) x f = BiMonad.bind f x
    let (<!>) = BiMonad.map
    let (<*>) = BiMonad.apply
    let (>=>) g f = Monad.compose g f
    let (>>=>>) g f = BiMonad.compose g f

    let defaultState name n = (TimeSeries.Univariate.defaultState n,MonadicGraph.defaultState name)

    let _activateModelM () skM = 
        let innerFunc (stateTS:TimeSeries.Univariate.State<'T>) (stateG:MonadicGraph.State<'T>) = 
            let result, nxtStateG = Monad.run skM stateG
            (), result, stateTS, nxtStateG
        BiMonad.M innerFunc
    
    // Variable update must be made at a specific time ! (Be careful of look-ahead bias).
    let rec _defineUpdatesTSM modelName = 
        let rec updateSequenceTSM = function
            | ARp(coeffs) -> [ for i in 1..coeffs.Length do TimeSeries.Univariate.elementAtLagM i ]
            | MAp(coeffs) -> [ for i in 1..coeffs.Length do TimeSeries.Univariate.innovationAtLagM i ] 
            | STARp(coeffs1,coeffs2,_,_,innerModelp) -> updateSequenceTSM (ARp(coeffs1)) @ updateSequenceTSM innerModelp        
        
        updateSequenceTSM modelName
        |> Monad.sequence
        |> Monad.map (Array.ofList)
        |> Monad.map (Array.map (fun x -> x |> Option.defaultValue 0.0))
        
    let _updateM updateSequenceTSM = // have to update the graph state via the TimeSeries Monad.
        BiMonad.modify (fun s1 (MonadicGraph.State(p,_,i)) -> let newVariables, _ = Monad.run updateSequenceTSM s1
                                                              s1, (MonadicGraph.State(p,newVariables,i)))
    let updateVariablesForSamplingM = _defineUpdatesTSM >> _updateM
    let updateVariablesForFittingM = MonadicGraph.convertModelToParameters >> updateVariablesForSamplingM
    
    let _setCurrentInnovationM () () = // must be set before updating variables.
    // Note : only update the innovations in TS with the first element of the innovation array in the graph state.
        BiMonad.modify (fun s1 (MonadicGraph.State(p,v,i)) -> let _, nxS1 = Monad.run (TimeSeries.Univariate.setCurrentInnovationM i.[0]) s1
                                                              nxS1, (MonadicGraph.State(p,v,i)))

    let _setCurrentElementM () x = BiMonad.modifyFirstWithMonad (TimeSeries.Univariate.setCurrentElementM x)
    let _stepM () () = BiMonad.modifyFirstWithMonad (TimeSeries.Univariate.stepM) 

    let _setCurrentErrorM () x = 
        BiMonad.modify (fun s1 s2 -> let currentElement, _ = Monad.run (TimeSeries.Univariate.currentElementM ()) s1
                                     let (TimeSeries.Univariate.State(idx,data,innov)) = s1
                                     innov.[idx] <- (currentElement |> Option.defaultValue 0.0) - x |> Some
                                     s1,s2)

    let rec conditionalExpectationM updateM skM steps () () = 
        if steps = 1 then
            _activateModelM () skM
        else
            (_activateModelM >>=>> _setCurrentElementM >>=>> _stepM >>=>> (fun _ _ -> updateM) >>=>> conditionalExpectationM updateM skM (steps-1)) () skM

    let sampleOnceM updateM skM = 
        (_activateModelM >>=>> _setCurrentElementM >>=>> _setCurrentInnovationM >>=>> _stepM >>=>> (fun _ _ -> updateM)) () skM

    let fitOnceM updateM skM = 
        (_activateModelM >>=>> _setCurrentErrorM >>=>> _stepM >>=>> (fun _ _ -> updateM)) () skM

    let foldRun m (TimeSeries.Univariate.State(idx,data,innov)) initStateG =
        data |> Array.fold (fun (s1,s2) x -> let _,_,nxS1,nxS2 = BiMonad.run m s1 s2
                                             (nxS1,nxS2)) 
                           ((TimeSeries.Univariate.State(idx,data,innov)),initStateG)

    let sample n = function
        | Sampling(mparameters) -> let initStateTS = TimeSeries.Univariate.defaultState n
                                   let initStateG = MonadicGraph.defaultStateForSampling mparameters
                                   let skM = MonadicGraph.modelM (Sampling(mparameters))
                                   let updteVarM = updateVariablesForSamplingM mparameters
                                   let samplingM = sampleOnceM updteVarM skM
                                   foldRun samplingM initStateTS initStateG |> fst
        | Fitting(m) -> invalidArg "model" "Cannot sample with a Fitting model type. Convert it to a Sampling type."

    let getError model array stateG = 
        let initStateTS = TimeSeries.Univariate.defaultStateFrom array
        let skM = MonadicGraph.modelM (Fitting(model))
        let updteVarM = updateVariablesForFittingM model
        let fittingM = fitOnceM updteVarM skM
        foldRun fittingM initStateTS stateG |> fst
            
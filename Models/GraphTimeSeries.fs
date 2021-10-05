namespace Models

open Monads

module GraphTS = 
    let (>>=) x f = Monad.bind f x
    let (<!>) = Monad.map
    let (<*>) = Monad.apply
    let (>=>) g f = Monad.compose g f

    // Variable update must be made at a specific time ! (Be careful of look-ahead bias).
    let rec defineUpdatesM modelName = 
        let rec updateSequenceTSM = function
            | ARp(coeffs) -> [| for i in 1..coeffs.Length do TimeSeries.Univariate.elementAtLagM i |]
            | MAp(coeffs) -> [| for i in 1..coeffs.Length do TimeSeries.Univariate.innovationAtLagM i |] 
            | STARp(coeffs1,coeffs2,_,_,innerModelp) -> Array.concat[|updateSequenceTSM (ARp(coeffs1)); updateSequenceTSM innerModelp|]       
        
        updateSequenceTSM modelName
        |> Monad.mapM
        |> Monad.map (Array.map (fun x -> x |> Option.defaultValue 0.0))

    let stateGraphM () = fst <!> Monad.get
    let stateTimeSeriesM () = snd <!> Monad.get

    let setStateGraphM stateG = Monad.M (fun (_,stateTS) -> (),(stateG,stateTS))
    let setStateTimeSeriesM stateTS = Monad.M (fun (stateG,_) -> (),(stateG,stateTS))

    let runGraphM m = 
        Monad.state {
            let! (r,s) = Monad.run m <!> stateGraphM ()
            do! setStateGraphM s
            return r
        }
                               
    let runTimeSeriesM m = 
        Monad.state {
            let! (r,s) = Monad.run m <!> stateTimeSeriesM ()
            do! setStateTimeSeriesM s
            return r
        }
    
    let sampleOnceM updateM skM = 
        Monad.state {
            let! x = runGraphM skM
            do! runTimeSeriesM (TimeSeries.Univariate.setCurrentElementM x)
            let! innov = runGraphM Graph.innovationsM
            do! runTimeSeriesM (TimeSeries.Univariate.setCurrentInnovationM innov.[0])
            do! runTimeSeriesM TimeSeries.Univariate.stepM
            let! newVar = runTimeSeriesM updateM
            do! runGraphM (Graph.setVariablesM newVar)
            return x
        }
                                        
        
        
    

module GraphTimeSeries = 

    let (>>=) x f = BiMonad.bind f x
    let (<!>) = BiMonad.map
    let (<*>) = BiMonad.apply
    let (>=>) g f = Monad.compose g f
    let (>>=>>) g f = BiMonad.compose g f

    let defaultState name n = (TimeSeries.Univariate.defaultState n, Graph.defaultState name)

    let stateM = 
        let innerFunc stateTS stateG = stateTS, stateG, stateTS, stateG
        BiMonad.M innerFunc

    let activateModelM () skM = 
        let innerFunc (stateTS:TimeSeries.Univariate.State<'T>) (stateG:Graph.State<'T>) = 
            let result, nxtStateG = Monad.run skM stateG
            (), result, stateTS, nxtStateG
        BiMonad.M innerFunc
    
    // Variable update must be made at a specific time ! (Be careful of look-ahead bias).
    let rec defineUpdatesTSM modelName = 
        let rec updateSequenceTSM = function
            | ARp(coeffs) -> [ for i in 1..coeffs.Length do TimeSeries.Univariate.elementAtLagM i ]
            | MAp(coeffs) -> [ for i in 1..coeffs.Length do TimeSeries.Univariate.innovationAtLagM i ] 
            | STARp(coeffs1,coeffs2,_,_,innerModelp) -> updateSequenceTSM (ARp(coeffs1)) @ updateSequenceTSM innerModelp        
        
        updateSequenceTSM modelName
        |> Monad.sequence
        |> Monad.map (Array.ofList)
        |> Monad.map (Array.map (fun x -> x |> Option.defaultValue 0.0))
        
    let updateM updateSequenceTSM = // have to update the graph state via the TimeSeries Monad.
        BiMonad.modify (fun s1 (Graph.State(p,_,i)) -> let newVariables, _ = Monad.run updateSequenceTSM s1
                                                       s1, (Graph.State(p,newVariables,i)))
    let updateVariablesForSamplingM = defineUpdatesTSM >> updateM
    let updateVariablesForFittingM = Graph.convertModelToParameters >> updateVariablesForSamplingM
    
    let setCurrentInnovationM () () = // must be set before updating variables.
    // Note : only update the innovations in TS with the first element of the innovation array in the graph state.
        BiMonad.modify (fun s1 (Graph.State(p,v,i)) -> let _, nxS1 = Monad.run (TimeSeries.Univariate.setCurrentInnovationM i.[0]) s1
                                                       nxS1, (Graph.State(p,v,i)))

    let setCurrentElementM () x = BiMonad.modifyFirstWithMonad (TimeSeries.Univariate.setCurrentElementM x)
    let stepM () () = BiMonad.modifyFirstWithMonad (TimeSeries.Univariate.stepM) 

    let setCurrentErrorM () x = 
        BiMonad.modify (fun s1 s2 -> let currentElement, _ = Monad.run (TimeSeries.Univariate.currentElementM ()) s1
                                     let (TimeSeries.Univariate.State(idx,data,innov)) = s1
                                     innov.[idx] <- (currentElement |> Option.defaultValue 0.0) - x |> Some
                                     s1,s2)

    let getCurrentErrorM () x = 
        // get the error (dependent on current prediction) at the current index and returns it. 
        let innerFunc stateTS stateG = 
            let currentElement, _ = Monad.run (TimeSeries.Univariate.currentElementM ()) stateTS
            let error = (currentElement |> Option.defaultValue 0.0) - x |> Some
            (), error, stateTS, stateG
        BiMonad.M innerFunc

    let setCurrentIndexM () idx = BiMonad.modifyFirstWithMonad (TimeSeries.Univariate.setCurrentIndexM idx)

    let rec conditionalExpectationM updateM skM steps () () = 
        if steps = 1 then
            activateModelM () skM
        else
            (activateModelM >>=>> setCurrentElementM >>=>> stepM >>=>> (fun _ _ -> updateM) >>=>> conditionalExpectationM updateM skM (steps-1)) () skM

    let sampleOnceM updateM skM = 
        (activateModelM >>=>> setCurrentElementM >>=>> setCurrentInnovationM >>=>> stepM >>=>> (fun _ _ -> updateM)) () skM

    let fitOnceM updateM skM = 
        (activateModelM >>=>> setCurrentErrorM >>=>> stepM >>=>> (fun _ _ -> updateM)) () skM
                          
    let foldRun m (TimeSeries.Univariate.State(idx,data,innov)) initStateG =
        data |> Array.fold (fun (s1,s2) x -> let _,_,nxS1,nxS2 = BiMonad.run m s1 s2
                                             (nxS1,nxS2)) 
                           ((TimeSeries.Univariate.State(idx,data,innov)),initStateG)

    let sample n = function
        | Sampling(mparameters) -> let initStateTS = TimeSeries.Univariate.defaultState n
                                   let initStateG = Graph.defaultStateForSampling mparameters
                                   let skM = Graph.modelM (Sampling(mparameters))
                                   let updteVarM = updateVariablesForSamplingM mparameters
                                   let samplingM = sampleOnceM updteVarM skM
                                   foldRun samplingM initStateTS initStateG |> fst
        | Fitting(m) -> invalidArg "model" "Cannot sample with a Fitting model type. Convert it to a Sampling type."

    let getError model array stateG = 
        let initStateTS = TimeSeries.Univariate.defaultStateFrom array
        let skM = Graph.modelM (Fitting(model))
        let updteVarM = updateVariablesForFittingM model
        let fittingM = fitOnceM updteVarM skM
        foldRun fittingM initStateTS stateG |> fst
            
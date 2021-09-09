namespace Models

open Monads

module GraphTimeSeries = 

    let (>>=) x f = BiMonad.bind f x
    let (<!>) = BiMonad.map
    let (<*>) = BiMonad.apply
    let (>=>) g f = Monad.compose g f
    let (>>=>>) g f = BiMonad.compose g f

    let defaultState name n = (TimeSeries.UnivariateTimeSeries.defaultState n,MonadicGraph.defaultState name)

    let _runModelM () skM = 
        let innerFunc (stateTS:TimeSeries.UnivariateTimeSeries.State<'T>) (stateG:MonadicGraph.State<'T>) = 
            let result, nxtStateG = Monad.run skM stateG
            (), result, stateTS, nxtStateG
        BiMonad.M innerFunc
    
    // Variable update must be made at a specific time ! (Be careful of look-ahead bias).
    let rec _defineUpdatingSequenceTSM modelName = 
        match modelName with
        | AR(order) -> [ for i in 1..order do TimeSeries.UnivariateTimeSeries.elementAtLagM i ]
        | MA(order) -> [ for i in 1..order do TimeSeries.UnivariateTimeSeries.innovationAtLagM i ] 
        // | SETAR(order,delay) -> Array.concat [|variableUpdate (AR(order)); [|TimeSeries.UnivariateTimeSeries.elementAtLagM delay|]|] |> Array.toList |> Monad.sequence
        |> Monad.sequence
        |> Monad.map (Array.ofList)
        |> Monad.map (Array.map (fun x -> x |> Option.defaultValue 0.0))
        
    let _updateVariablesWithSequenceM updateSequenceTSM = // have to update the graph state via the TimeSeries Monad.
        BiMonad.modify (fun s1 (MonadicGraph.State(p,_,i)) -> let newVariables, _ = Monad.run updateSequenceTSM s1
                                                              s1, (MonadicGraph.State(p,newVariables,i)))

    let updateVariablesM = _defineUpdatingSequenceTSM >> _updateVariablesWithSequenceM
    
    let _updateInnovationM () () = // must be set before updating variables.
        BiMonad.modify (fun s1 (MonadicGraph.State(p,v,i)) -> let _, nxS1 = Monad.run (TimeSeries.UnivariateTimeSeries.setCurrentInnovationM i.[0]) s1
                                                              nxS1, (MonadicGraph.State(p,v,i)))

    let _setCurrentElementM () x = BiMonad.modifyWithMonads (TimeSeries.UnivariateTimeSeries.setCurrentElementM x) (Monad.rets ())
        
    let _stepM () () = BiMonad.modifyWithMonads (TimeSeries.UnivariateTimeSeries.stepM) (Monad.rets ())

    let sampleOnceM updateM skM = 
        (_runModelM >>=>> _setCurrentElementM >>=>> _updateInnovationM >>=>> _stepM >>=>> (fun _ _ -> updateM)) () skM

    let foldRun m (TimeSeries.UnivariateTimeSeries.State(idx,data,innov)) initStateG =
        data |> Array.fold (fun (s1,s2) x -> let _,_,nxS1,nxS2 = BiMonad.run m s1 s2
                                             (nxS1,nxS2)) 
                           ((TimeSeries.UnivariateTimeSeries.State(idx,data,innov)),initStateG)
            
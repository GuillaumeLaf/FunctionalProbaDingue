namespace Models

open Monads

module GraphTimeSeries = 

    let defaultState name n = (TimeSeries.UnivariateTimeSeries.defaultState n,MonadicGraph.defaultState name)

    let StatesM = 
        let innerFunc stateTS stateG = stateTS, stateG, stateTS, stateG
        BiMonad.M innerFunc

    let runModelM graphM = 
        let innerFunc (stateTS:TimeSeries.UnivariateTimeSeries.State<'T>) (stateG:MonadicGraph.State<'T>) = 
            let result, nxtStateG = Monad.run graphM stateG
            (), result, stateTS, nxtStateG
        BiMonad.M innerFunc
    
    // Variable update must be made at a specific time ! (Be careful of look-ahead bias).
    let rec variableUpdateSequenceTSM modelName = 
        match modelName with
        | AR(order) -> [ for i in 1..order do TimeSeries.UnivariateTimeSeries.elementAtLagM i ]
        | MA(order) -> [ for i in 1..order do TimeSeries.UnivariateTimeSeries.innovationAtLagM i ] 
        // | SETAR(order,delay) -> Array.concat [|variableUpdate (AR(order)); [|TimeSeries.UnivariateTimeSeries.elementAtLagM delay|]|] |> Array.toList |> Monad.sequence
        |> Monad.sequence
        |> Monad.map (Array.ofList)
        |> Monad.map (Array.map (fun x -> x |> Option.defaultValue 0.0))
        
    let updateVariablesM updateSequenceTSM = // have to update the graph state via the TimeSeries Monad.
        BiMonad.modify (fun s1 (MonadicGraph.State(p,_)) -> let result, _ = Monad.run updateSequenceTSM s1
                                                            s1, (MonadicGraph.State(p,result)))

            
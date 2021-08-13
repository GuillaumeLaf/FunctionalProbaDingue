namespace Backtester

open System
open XPlot.Plotly

module M =
    let run (M f) initialState = f initialState

    let map f m = 
        let innerFunc state = 
            let innerType, nextState = run m state
            (f innerType), nextState
        M innerFunc

    let rets x = 
        let innerFunc state = x, state
        M innerFunc

    let bind f m = 
        let innerFunc state = 
            let innerType, nextState = run m state
            run (f innerType) nextState
        M innerFunc

    let apply mf mx = 
        let innerFunc state = 
            let f, nextState = run mf state
            let x, nextState2 = run mx nextState
            (f x), nextState2
        M innerFunc

module Backtester =
    let (<*>) = M.apply
    let (<!>) = M.map

    let makeTradingSignalUpdateM = 
        let innerFunc (state:State) =
            match state.strategy with
            | NaiveStrategy -> TradingSignalStrategies.naiveStrategy state
            | ModelStrategy(m) -> TradingSignalStrategies.ModelStrategy m state
        M innerFunc
            
    let makePositionSizeUpdateM = 
        let innerFunc (state:State) = 
            match state.positionSize with
            | PositionSize(NaivePosition, _) -> PositionSizingStrategies.naiveStrategy state
        M innerFunc
   
    let makeStrategyUpdateM = 
        let innerFunc (state:State) = 
            match state.strategy with
            | NaiveStrategy as x -> x, state
            | ModelStrategy(m) as x -> UpdateStrategyStrategies.ModelStrategy m state
        M innerFunc 

    let makeSettingsUpdateM (data:DataElement array) = 
        let innerFunc (state:State) = 
            Array.last data, {state with data=data}
        M innerFunc

    let createBacktestPoint settingsUpdate strategy tradingSignal positionSize = 
         {currentData=settingsUpdate
          strategy=strategy
          tradingSignal=tradingSignal
          positionSize=positionSize}

    let backtestPointM data = 
        createBacktestPoint 
        <!> (makeSettingsUpdateM data) 
        <*> makeStrategyUpdateM 
        <*> makeTradingSignalUpdateM 
        <*> makePositionSizeUpdateM 

    let run ({strategy=strategy; positionSizeStrategy=positionSizeStrategy;windowSize=windowSize}) array = 
        let windowedData = array |> Data.fromArray |> Array.windowed windowSize
        let initialState = {data=windowedData.[0];strategy=strategy;tradingSignal=NoSignal;positionSize=PositionSize(positionSizeStrategy,0.0)}

        let folder state x = 
            let result, newState = M.run (backtestPointM x) (fst state)
            (newState, result :: (snd state))

        windowedData |> Array.fold folder (initialState,[])
                     |> snd
                     |> List.toArray

    let computePnL backtestData = 
        let getPosition (PositionSize(_,p)) = p
        Array.map (fun x -> x.positionSize |> getPosition |> ( * ) x.currentData.logReturn) backtestData

    let computePositionHistory backtestData = 
        let getPosition (PositionSize(_,p)) = p
        Array.map (fun x -> x.positionSize |> getPosition) backtestData
                                   
                                   
                                   


namespace Backtester

open System
open Monads

module Backtester =
    let (<*>) = Monad.apply
    let (<!>) = Monad.map

    let makeTradingSignalUpdateM = 
        let innerFunc (state:State) =
            match state.strategy with 
            | NaiveStrategy -> TradingSignalStrategies.naiveStrategy state
            | ModelStrategy(m) -> TradingSignalStrategies.ModelStrategy m state
        Monad.M innerFunc
            
    let makePositionSizeUpdateM = 
        let innerFunc (state:State) = 
            match state.positionSize with
            | PositionSize(NaivePosition, _) -> PositionSizingStrategies.naiveStrategy state
        Monad.M innerFunc
   
    let makeStrategyUpdateM = 
        let innerFunc (state:State) = 
            match state.strategy with
            | NaiveStrategy as x -> x, state
            | ModelStrategy(m) as x -> UpdateStrategyStrategies.ModelStrategy m state
        Monad.M innerFunc 

    let makeSettingsUpdateM (data:DataElement array) = 
        let innerFunc (state:State) = 
            Array.last data, {state with data=data}
        Monad.M innerFunc

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

(*    let run ({strategy=strategy; positionSizeStrategy=positionSizeStrategy;windowSize=windowSize}) (TimeSeries.Univariate.State(idx,data,innovation)) = 
        let windowedData = data |> Array.map (fun x -> Option.defaultValue 0.0 x) |> Data.fromArray |> Array.windowed windowSize
        let initialState = {data=windowedData.[0];strategy=strategy;tradingSignal=NoSignal;positionSize=PositionSize(positionSizeStrategy,0.0)}

        let folder state x = 
            let result, newState = Monad.run (backtestPointM x) (fst state)
            (newState, result :: (snd state))

        windowedData |> Array.fold folder (initialState,[])
                     |> snd
                     |> List.toArray*)

    let computePnL backtestData = 
        let getPosition (PositionSize(_,p)) = p
        Array.map (fun x -> x.positionSize |> getPosition |> ( * ) x.currentData.logReturn) backtestData

    let computePositionHistory backtestData = 
        let getPosition (PositionSize(_,p)) = p
        Array.map (fun x -> x.positionSize |> getPosition) backtestData
                                   
                                   
                                   


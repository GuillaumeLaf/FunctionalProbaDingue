namespace Backtester

open Models

module TradingSignalStrategies = 
    let naiveStrategy (state:State) = 
        match (Array.last state.data).transformedData with
        | x when x > 0.0 -> Entry, {state with tradingSignal=Entry}
        | x when x <= 0.0 -> Exit, {state with tradingSignal=Exit}
        | _ -> NoSignal, {state with tradingSignal=NoSignal}

    let ModelStrategy m (state:State) = 
        // let transformedDataArray = Array.map (fun x -> x.transformedData) state.data
        match (Model.conditionalExpectation 1 m).[0] with
        | x when x > 0.0 -> Entry, {state with tradingSignal=Entry}
        | x when x <= 0.0 -> Exit, {state with tradingSignal=Exit}
        | _ -> NoSignal, {state with tradingSignal=NoSignal}
            
module PositionSizingStrategies = 
    let naiveStrategy (state:State) = 
        match state.tradingSignal, state.positionSize with
        | Entry, PositionSize(_, 0.0) -> PositionSize(NaivePosition, 1.0), {state with positionSize=PositionSize(NaivePosition, 1.0)}
        | Entry, _ -> state.positionSize, state
        | Exit, PositionSize(_, 0.0) -> state.positionSize, state
        | Exit, _ -> PositionSize(NaivePosition, 0.0), {state with positionSize=PositionSize(NaivePosition, 0.0)}
        | NoSignal, _ -> state.positionSize, state

module UpdateStrategyStrategies = 
    let ModelStrategy m (state:State) = 
        let transformedDataArray = Array.map (fun x -> x.transformedData) state.data
        let newFittedModel = Model.fit transformedDataArray m
        ModelStrategy(newFittedModel), {state with strategy=ModelStrategy(newFittedModel)}
        

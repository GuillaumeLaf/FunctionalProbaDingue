namespace Backtester

open Models

[<AutoOpen>]
module BacktesterTypes = 
    
    type TimeStep = int

    type TradingSignal = 
        | Entry 
        | Exit
        | NoSignal

    type PositionSizeStrategy = 
        | NaivePosition

    type PositionSize = PositionSize of PositionSizeStrategy * float

    type Strategy = 
        | NaiveStrategy
        | ModelStrategy of ModelsTypes.T

    type DataElement = {time:TimeStep
                        price:float
                        logReturn:float // Return would get if buy ("next period return")
                        transformedData:float} 

    type State = {data:DataElement array
                  strategy:Strategy
                  tradingSignal:TradingSignal
                  positionSize:PositionSize}

    type BacktestPoint = {currentData:DataElement
                          strategy:Strategy
                          tradingSignal:TradingSignal
                          positionSize:PositionSize}

    type BacktestSettings = {strategy:Strategy
                             positionSizeStrategy:PositionSizeStrategy
                             windowSize:int}

    

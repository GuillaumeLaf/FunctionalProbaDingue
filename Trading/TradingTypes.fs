namespace Trading

open Models
open Timeseries

module TradingTypes = 
    
    module Close = 
        module Parametric = 
            // State for the Closing Strategy with Parametric Models
            [<RequireQualifiedAccess;Struct>]
            type S = 
                | AfterNperiods of n:int

        module NonParametric = 
            // State for the Closing Strategy with NonParametric Models
            [<RequireQualifiedAccess;Struct>]
            type S = 
                | AfterNperiods of n:int

        [<RequireQualifiedAccess;Struct>]
        type S = 
            | Parametric of parametric:Parametric.S
            | NonParametric of nonparametric:NonParametric.S

    module Open = 
        module Parametric = 
            // State for the Opening Strategy with Parametric Models
            [<RequireQualifiedAccess;Struct>]
            type S = 
                | PreviousPositive of n:float32
        module NonParametric = 
            // State for the Opening Strategy with NonParametric Models
            [<RequireQualifiedAccess;Struct>]
            type S = 
                | PreviousPositive of n:float32

        [<RequireQualifiedAccess;Struct>]
        type S = 
            | Parametric of parametric:Parametric.S
            | NonParametric of nonparametric:NonParametric.S

    module PositionSize = 
        module Parametric = 
            // State for the Position Sizing with Parametric Models
            [<RequireQualifiedAccess;Struct>]
            type S = 
                | Fixed of f:float32
        module NonParametric = 
            // State for the Position Sizing with NonParametric Models
            [<RequireQualifiedAccess;Struct>]
            type S = 
                | Fixed of f:float32

        [<RequireQualifiedAccess;Struct>]
        type S = 
            | Parametric of parametric:Parametric.S
            | NonParametric of nonparametric:NonParametric.S

    type CloseStrategy = 
        | AfterNperiods of N:int
    
    type OpenStrategy = 
        | PreviousPositive
    
    type Sizing = 
        | Fixed of c:float32  // Invest a fixed amount of Equity in each trade

    // Decides under which form the 'TS' is stored
    // Parametric -> use of an econometric model for the 'TS'
    // NonParametric -> use general methods to trade (moving average, bollinger bands, ...)
    type PriceAction = 
        | Parametric of ModelType.S 
        | NonParametric of (int*TimeseriesType.TS)

    type Trades = Trades of TimeseriesType.TS
    type Equity = Equity of TimeseriesType.TS

    type History = History of Equity * Trades

    [<RequireQualifiedAccess>]
    // Each 'TS' could have a different strategy -> array of strategies 'Open', 'Close' and 'PositionSize'.
    // State of the strategy contains the information set on which we base any trading decisions.
    type S = S of History * PriceAction * Open.S[] * Close.S[] * PositionSize.S[]

    let openTrade = 0
    let closeTrade = 0
    let modifyPosition = 0


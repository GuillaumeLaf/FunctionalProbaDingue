namespace Timeseries

module TimeseriesType =
    
    type TS = { length:int;
                size:int;
                data:float32[,];
                stats:Statistics.Multivariate.Stats }


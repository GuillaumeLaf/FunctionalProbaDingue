namespace Timeseries

module TimeseriesType =

    // Record Type representing available statistics.
    // 'Stats' does not contain the data only statistics about some data.
    // Therefore the data holder must have a 'Stats' object. 
    type Stats = 
        { Mean:float32[] option;
          Std:float32[] option;
          Cov:float32[,] option;
          LowerCholeskyCov:float32[,] option }

        static member create = 
            { Mean=None; Std=None; Cov=None; LowerCholeskyCov=None }
        static member mean s = s.Mean
        static member std s = s.Std
        static member cov s = s.Cov
        static member lowerCholeskyCov s = s.LowerCholeskyCov

        static member setMean x s = { s with Mean=Some x }
        static member setStd x s = { s with Std=Some x }
        static member setCov x s = { s with Cov=Some x }
        static member setLowerCholeskyCov x s = { s with LowerCholeskyCov=Some x } 
                    
    // Record Type representing a MULTIVARIATE timeseries. 
    type TS = 
        { Length:int;
          Size:int;
          Data:float32[,];
          Stats:Stats }

        static member create (data:float32[,]) = 
            { Length=data.[0,*].Length; 
              Size=data.[*,0].Length;
              Data=data;
              Stats=Stats.create }
        static member length ts = ts.Length
        static member size ts = ts.Size
        static member data ts = ts.Data
        static member stats ts = ts.Stats

        static member setData x ts = { ts with Data=x }
        static member setStats x ts = { ts with Stats=x }


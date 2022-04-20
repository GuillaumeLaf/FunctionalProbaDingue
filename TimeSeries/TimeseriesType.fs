namespace Timeseries

open FSharp.Charting

module TimeseriesType =

    // Record Type representing available statistics.
    // 'Stats' does not contain the data only statistics about some data.
    // Therefore the data holder must have a 'Stats' object. 
    // General option type indicates if the given statistic has already been computed
    type Stats = 
        { Mean:float32[] option;
          Var:float32[] option;
          Cov:float32[,] option;
          LowerCholeskyCov:float32[,] option }

        static member create = 
            { Mean=None; Var=None; Cov=None; LowerCholeskyCov=None }
        static member mean s = s.Mean
        static member var s = s.Var
        static member cov s = s.Cov
        static member lowerCholeskyCov s = s.LowerCholeskyCov

        static member setMean x s = { s with Mean=Some x }
        static member setVar x s = { s with Var=Some x }
        static member setCov x s = { s with Cov=Some x }
        static member setLowerCholeskyCov x s = { s with LowerCholeskyCov=Some x } 

    // Type of possible 'Transformation' for a 'TS'.
    // Keep relevant information to - possibly - inverse the transform.
    type Transformation = 
        | FracDifference of ds:float32[] option * thresh:float32
        | Center of means:float32 option[] option
        | Standardize of stds:float32 option[] option
        | TotalDifference
                    
    // Record Type representing a MULTIVARIATE timeseries. 
    type TS = 
        { Length:int;
          Size:int;
          Data:float32 option[,];
          Stats:Stats;
          Transformation:Transformation list }

        static member create (data:float32[,]) = 
            { Length=data.[0,*].Length; // T
              Size=data.[*,0].Length;   // N
              Data=Array2D.toOption data;
              Stats=Stats.create;
              Transformation=[] }
        static member length ts = ts.Length
        static member size ts = ts.Size
        static member data ts = ts.Data
        static member stats ts = ts.Stats
        static member transformation ts = ts.Transformation

        static member setData x ts = { ts with Data=x }
        static member setStats x ts = { ts with Stats=x }
        static member addTransformation x ts = { ts with Transformation=x::ts.Transformation }


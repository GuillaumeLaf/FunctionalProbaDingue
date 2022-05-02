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
        | TotalDifference of first:float32 option[] option
        | DefaultWith of value:float32 * indices:int list[] option       // replace 'None' by some constant and save idx where that happened
        | Apply of f:(float32 -> float32) * invf:(float32 -> float32)
                    
    // Record Type representing a MULTIVARIATE timeseries. 
    type TS = 
        { Length:int;   // T
          Size:int;     // N
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
        static member dataDefault ts = Array2D.map (Option.defaultValue 0.0f) ts.Data
        static member stats ts = ts.Stats
        static member transformation ts = ts.Transformation

        static member setData x ts = { ts with Data=x; Length=x.[0,*].Length; Size=x.[*,0].Length }
        static member setStats x ts = { ts with Stats=x }
        static member addTransformation x ts = { ts with Transformation=x::ts.Transformation }
        static member popTransformation ts = { ts with Transformation=List.tail ts.Transformation }

        static member zeroCreate i j = Array2D.zeroCreate i j |> TS.create
        static member zero_like ts = TS.zeroCreate (TS.size ts) (TS.length ts)

        // Get the 'idx'th timeseries
        static member get idx ts = ts.Data[idx,*]
        static member atTime t ts = if (0 <= t) && (t < ts.Length) then ts.Data.[*,t] else (Array.create ts.Size (Some 0.0f)) // 'Array.zeroCreate ts.Size' gives an array of 'None'
        static member modifyAtTime t values ts = if (0 <= t || t < ts.Length) then { ts with Data= Utils.Array2D.setColumn t values ts.Data} else invalidArg "Index" "Time index greater than length of Timeseries."

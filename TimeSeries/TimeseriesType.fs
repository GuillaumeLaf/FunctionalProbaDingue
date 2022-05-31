namespace Timeseries

open FSharp.Charting

module TimeseriesType =

    // Record Type representing available statistics.
    // 'Stats' does not contain the data only statistics about some data.
    // Therefore the data holder must have a 'Stats' object. 
    // General option type indicates if the given statistic has already been computed
    type Stats< 'T > = 
        { Mean:'T[] option;
          Var:'T[] option;
          Cov:'T[,] option;
          LowerCholeskyCov:'T[,] option }

        static member inline create : Stats< 'T > = 
            { Mean=None; Var=None; Cov=None; LowerCholeskyCov=None }
        static member inline mean s = s.Mean
        static member inline var s = s.Var
        static member inline cov s = s.Cov
        static member inline lowerCholeskyCov s = s.LowerCholeskyCov

        static member inline setMean x s = { s with Mean=Some x }
        static member inline setVar x s = { s with Var=Some x }
        static member inline setCov x s = { s with Cov=Some x }
        static member inline setLowerCholeskyCov x s = { s with LowerCholeskyCov=Some x } 

    // Type of possible 'Transformation' for a 'TS'.
    // Keep relevant information to - possibly - inverse the transform.
    type Transformation< 'T > = 
        | FracDifference of ds:'T[] option * thresh:'T
        | Center of means:'T[] option
        | Standardize of stds:'T[] option
        | TotalDifference of first:'T[] option
        | Apply of f:('T -> 'T) * invf:('T -> 'T)
                    
    // Record Type representing a MULTIVARIATE timeseries. 
    type TS< 'T when 'T : (static member Zero : 'T) > = 
        { Length:int;   // T
          Size:int;     // N
          Data:'T[,];
          Stats:Stats< 'T >;
          Transformation:Transformation< 'T > list }



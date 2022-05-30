namespace Timeseries

open FSharp.Charting

module TimeseriesType =

    // Record Type representing available statistics.
    // 'Stats' does not contain the data only statistics about some data.
    // Therefore the data holder must have a 'Stats' object. 
    // General option type indicates if the given statistic has already been computed
    type Stats< ^T > = 
        { Mean:^T[] option;
          Var:^T[] option;
          Cov:^T[,] option;
          LowerCholeskyCov:^T[,] option }

        static member inline create : Stats< ^T > = 
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
    type Transformation< ^T > = 
        | FracDifference of ds:^T[] option * thresh:^T
        | Center of means:^T[] option
        | Standardize of stds:^T[] option
        | TotalDifference of first:^T[] option
        | Apply of f:(^T -> ^T) * invf:(^T -> ^T)
                    
    // Record Type representing a MULTIVARIATE timeseries. 
    // when ^T:(static member get_Zero: unit -> ^T)
    // LanguagePrimitives.GenericZero
    type TS< ^T > = 
        { Length:int;   // T
          Size:int;     // N
          Data:^T[,];
          Stats:Stats< ^T >;
          Transformation:Transformation< ^T > list }

        static member inline create (data:^T[,]) = 
            { Length=data.[0,*].Length; // T
              Size=data.[*,0].Length;   // N
              Data=data;
              Stats=Stats< ^T >.create;
              Transformation=[] }
        static member inline length (ts:TS< ^T >) = ts.Length
        static member inline size (ts:TS< ^T >) = ts.Size
        static member inline data (ts:TS< ^T >) = ts.Data
        static member inline dataDefault (ts:TS< ^T >) = 
            Array2D.map (fun _ -> Unchecked.defaultof< ^T >) ts.Data
        static member inline stats (ts:TS< ^T >) = ts.Stats
        static member inline transformation (ts:TS< ^T >) = ts.Transformation
        static member inline pctLength (pct:float32) (ts:TS< ^T >) = (float32 ts.Length) * (pct/100f) |> int

        static member inline setData x (ts:TS< ^T >) = { ts with Data=x; Length=x.[0,*].Length; Size=x.[*,0].Length }
        static member inline setStats x (ts:TS< ^T >) = { ts with Stats=x }
        static member inline addTransformation x (ts:TS< ^T >) = { ts with Transformation=x::ts.Transformation }
        static member inline popTransformation (ts:TS< ^T >) = { ts with Transformation=List.tail ts.Transformation }

        // The type " 'T " must have a default value 
        static member inline zeroCreate (i:int) (j:int) = Array2D.zeroCreate i j |> TS< ^T >.create
        // static member inline zeroCreateOption i j = Array2D.zeroCreate<'T> i j |> Array2D.map Some |> TS.create 
        static member inline zero_like (ts:TS< ^T >) = TS<'T>.zeroCreate (TS<'T>.size ts) (TS<'T>.length ts)
        // static member inline zero_likeOption ts = TS<'T>.zeroCreateOption (TS<'T>.size ts) (TS<'T>.length ts)
        static member inline sub idx1 idx2 (ts:TS< ^T >) = (TS<'T>.data >> Array2D.sub idx1 idx2 >> TS.create) ts

        // Get the 'idx'th timeseries
        static member inline get idx (ts:TS< ^T >) = ts.Data[idx,*]
        
        static member inline atTime (t:int) (ts:TS< ^T >) = 
            if (0 <= t) && (t < ts.Length) then ts.Data.[*,t] else (Array.create ts.Size Unchecked.defaultof< ^T >)

        static member inline modifyAtTime t values (ts:TS< ^T >) = if (0 <= t || t < ts.Length) then { ts with Data= Utils.Array2D.setColumn t values ts.Data} else invalidArg "Index" "Time index greater than length of Timeseries."











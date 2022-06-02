namespace Timeseries

open TimeseriesType

[<RequireQualifiedAccess>]
module TS = 

    let inline length (ts:TS< 'T >) = ts.Length
    let inline size (ts:TS< 'T >) = ts.Size
    let inline data (ts:TS< 'T >) = ts.Data
    let inline stats (ts:TS< 'T >) = ts.Stats
    let inline transformation (ts:TS< 'T >) = ts.Transformation

    let inline setData x (ts:TS< 'T >) = { ts with Data=x; Length=x.[0,*].Length; Size=x.[*,0].Length }
    let inline setStats x (ts:TS< 'T >) = { ts with Stats=x }

    let inline create (data:'T[,]) = 
        { Length=data.[0,*].Length; // T
          Size=data.[*,0].Length;   // N
          Data=data;
          Stats=Stats< 'T >.create;
          Transformation=[] }

    let inline dataDefault< ^T when ^T : (static member Zero : ^T)> (ts:TS< ^T >) = 
        Array2D.map (fun _ -> LanguagePrimitives.GenericZero< ^T >) ts.Data

    let inline pctLength (pct:float32) (ts:TS< 'T >) = (float32 ts.Length) * (pct/100f) |> int

    let inline addTransformation x (ts:TS< 'T >) = { ts with Transformation=x::ts.Transformation }

    let inline popTransformation (ts:TS< 'T >) = { ts with Transformation=List.tail ts.Transformation }

    // The type " 'T " must have a default value 
    let inline zeroCreate (i:int) (j:int) = Array2D.zeroCreate i j |> create
    
    let inline zero_like (ts:TS< 'T >) = zeroCreate (size ts) (length ts)
    
    let inline sub idx1 idx2 (ts:TS< 'T >) = data >> Array2D.sub idx1 idx2 >> create <| ts

    // Get the 'idx'th timeseries
    let inline get idx (ts:TS< 'T >) = ts.Data[idx,*]
        
    let inline atTime< ^T when ^T : (static member Zero : ^T)> (t:int) (ts:TS< 'T >) = 
        if (0 <= t) && (t < ts.Length) then ts.Data.[*,t] else (Array.create ts.Size (LanguagePrimitives.GenericZero< ^T >))

    let inline modifyAtTime t values (ts:TS< 'T >) = if (0 <= t || t < ts.Length) then { ts with Data= Utils.Array2D.setColumn t values ts.Data} else invalidArg "Index" "Time index greater than length of Timeseries."












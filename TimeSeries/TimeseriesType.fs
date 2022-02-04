namespace Timeseries

module TimeseriesType =

    type Stats = 
        { Mean:float32[] option;
          Std:float32[] option }

        static member create = 
            { Mean=None; Std=None }
        static member mean s = s.Mean
        static member std s = s.Std

        static member setMean x s = { s with Mean=Some x }
        static member setStd x s = { s with Std=Some x }
                    
    
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



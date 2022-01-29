namespace TimeSeries

open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics

module Statistics = 
    // Module to create descriptive - and other - statistics about a timeseries. 
    // The module treats 'Array's and 'Array2D's not 'TS' (timeseries).

    module Univariate = 
        // Module for univariate timeseries statistics.
        type Stats (tsIn:float32[]) = 
            // Timeseries data but MUST NOT be modified
            let data = tsIn

            // Allow scalar memoization of the computed statistics
            // The results must be scalars
            let mem : float32 option [] = [| for _ in 1..3 do None |]

            // Memoization function specialized for descriptive statistics of MathNet.Numerics
            let _memoize idx f = 
                match mem.[idx] with
                | Some(x) -> x
                | None -> let x = f(data)
                          mem.[idx] <- Some x
                          x

            // Compute the mean
            member this.Mean = _memoize 0 (Statistics.Mean >> float32)

            // Compute the standard deviation
            member this.Std = _memoize 1 (Statistics.StandardDeviation >> float32)

            // Compute the variance
            member this.Var = _memoize 2 (fun _ -> this.Std * this.Std)

            static member mean (s:Stats) = s.Mean
            static member std (s:Stats) = s.Std
            static member var (s:Stats) = s.Var

    module Multivariate = 
        // Module for multivariate timeseries statistics.
        type Stats (tsIn:float32[,]) = 
            // Timeseries data but MUST NOT be modified
            // Fst : Cross-section dimension
            // Snd : Time dimension
            let data = tsIn

            // Cross-section and time dimensions
            let n = Array2D.length1 data
            let t = Array2D.length2 data

            // Allow 'Array' memoization of the computed statistics
            // The stored results must be 'Array's
            let mem : float32 [] option[] = [| for _ in 1..3 do None |]

            // Allow 'Array2D' memoization of the computed statistics
            // The stored results must be 'Array2D's
            let mem2D : float32 [,] option [] = [| for _ in 1..2 do None |]

            // Univariate Timeseries 'Stats' for each timeseries in the array2d
            let stats = Array.init n (fun idx -> Univariate.Stats(data.[idx,*]))

            // Memoization function for storing 'Array' results.
            // 'f' is a function that will be applied to each 'Univariate.Stats' in the 'stats' array.
            let memoize1D idx f = 
                match mem.[idx] with
                | Some(x) -> x
                | None -> let x = Array.collect (f >> Array.singleton) stats
                          mem.[idx] <- Some x
                          x

            // Memoization function for storing 'Array2D' results.
            // 'f' is a function that will be applied to the 'data' array and return an 'Array2D'.
            let memoize2D idx f = 
                match mem2D.[idx] with
                | Some(x) -> x
                | None -> let x = f(data)
                          mem2D.[idx] <- Some x
                          x

            // Compte the mean (of each timeseries)
            member this.Means = memoize1D 0 Univariate.Stats.mean

            // Compte the standard deviation (of each timeseries)
            member this.Stds = memoize1D 1 Univariate.Stats.std

            // Compte the variance (of each timeseries)
            member this.Vars = memoize1D 2 Univariate.Stats.var

            // // Compte the covariance matrix
            member this.Covs = memoize2D 0 (fun (data:float32[,]) -> Array2D.zeroCreate n n |> Array2D.mapi (fun i j _ -> Statistics.Covariance(data.[i,*],data.[j,*]) |> float32))

            member this.CholeskyLowerCovs = memoize2D 1 cholesky

            member this.AddCovs = Some >> Array.set mem2D 0 

            member this.HasCovs = not ((mem2D.[0]) = None)

            static member means (s:Stats) = s.Means
            static member stds (s:Stats) = s.Stds
            static member vars (s:Stats) = s.Vars
            static member covs (s:Stats) = s.Covs


            



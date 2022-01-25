namespace TimeSeries

//open MathNet.Numerics
open MathNet.Numerics.Statistics

module Statistics = 

    module Univariate = 
        type Stats (tsIn:float32[]) = 
            let data = tsIn
            let mem : float32 option [] = [| for _ in 1..3 do None |]

            let _memoize idx f = 
                match mem.[idx] with
                | Some(x) -> x
                | None -> let x = (f >> float32)(data)
                          mem.[idx] <- Some x
                          x

            member this.mean = _memoize 0 Statistics.Mean

            member this.std = _memoize 1 Statistics.StandardDeviation

            member this.var = _memoize 2 (fun _ -> this.std * this.std |> float)

            static member Mean (s:Stats) = s.mean
            static member Std (s:Stats) = s.std
            static member Var (s:Stats) = s.var

    module Multivariate = 
        type Stats (tsIn:float32[,]) = 
            let data = tsIn
            let n = Array2D.length1 data
            let t = Array2D.length2 data
            let mem : float32 [] option[] = [| for _ in 1..3 do None |]
            let mem2D : float32 [,] option [] = [| for _ in 1..1 do None |]
            let stats = Array.init n (fun idx -> Univariate.Stats(data.[idx,*]))

            let _memoize1D idx f = 
                match mem.[idx] with
                | Some(x) -> x
                | None -> let x = Array.collect (f >> Array.singleton) stats
                          mem.[idx] <- Some x
                          x

            let _memoize2D idx f = 
                match mem2D.[idx] with
                | Some(x) -> x
                | None -> let x = f(data)
                          mem2D.[idx] <- Some x
                          x

            member this.means = _memoize1D 0 Univariate.Stats.Mean

            member this.stds = _memoize1D 1 Univariate.Stats.Std

            member this.vars = _memoize1D 2 Univariate.Stats.Var

            member this.covs = _memoize2D 0 (fun (data:float32[,]) -> Array2D.zeroCreate n n |> Array2D.mapi (fun i j _ -> Statistics.Covariance(data.[i,*],data.[j,*]) |> float32))

            static member Means (s:Stats) = s.means
            static member Stds (s:Stats) = s.stds
            static member Vars (s:Stats) = s.vars
            static member Covs (s:Stats) = s.covs


            



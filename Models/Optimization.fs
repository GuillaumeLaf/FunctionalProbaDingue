namespace Models

module Optimization = 
    open MathNet.Numerics.Optimization
    open MathNet.Numerics.Differentiation
    open MathNet.Numerics.LinearAlgebra

    module Bounds = 
        type BoundType<'T> = 
            | Continuous of 'T * 'T
            | Discrete of 'T[]

        let ofModel name parameterArray = 
            match name with
            | AR -> parameterArray |> Array.map (fun _ -> (-1.0,1.0) |> Continuous)
            | MA -> parameterArray |> Array.map (fun _ -> (-1.0,1.0) |> Continuous)
            | SETAR -> let coeffs12 = Array.zeroCreate (parameterArray.Length - 2) |> Array.map (fun _ -> (-1.0,1.0) |> Continuous)
                       let delay = Array.init 11 (fun i -> float(i + 1)) |> Discrete
                       let threshold = Array.init 61 (fun i -> float(i-30)*0.1) |> Discrete // Space defined by Tchebychev inequality. Must be rescaled before use !
                       Array.concat [|coeffs12;[|threshold|];[|delay|]|]

        let continuousBoundsIndexed bounds = 
            bounds |> Array.indexed
                   |> Array.choose (function
                                        | i, Continuous(mn,mx) -> Some (i, (mn,mx))
                                        | _ -> None)

        let discreteBoundsIndexed bounds = 
            bounds |> Array.indexed
                   |> Array.choose (function
                                        | i, Discrete(pts) -> Some (i, pts)
                                        | _ -> None)
                                        
    module Params = 
        type ParameterTier = 
            | Tier1
            | Tier2

        type ParameterType<'T> = 
            | ContinuousParameter of 'T * Bounds.BoundType<'T> * ParameterTier
            | DiscreteParameter of 'T * Bounds.BoundType<'T> * ParameterTier

        let ofModel name parameterArray = 
            let bounds = Bounds.ofModel name parameterArray
            match name with
            | AR -> parameterArray |> Array.mapi (fun i x -> ContinuousParameter(x,bounds.[i],Tier1))
            | MA -> parameterArray |> Array.mapi (fun i x -> ContinuousParameter(x,bounds.[i],Tier1))
            | SETAR -> let coeffs12, threshDelay = parameterArray |> Array.splitAt (parameterArray.Length-2)
                       let coeffs12 = coeffs12 |> Array.mapi (fun i x -> ContinuousParameter(x,bounds.[i],Tier1))
                       let threshDelay = threshDelay |> Array.mapi (fun i x -> DiscreteParameter(x,bounds.[i],Tier2))
                       Array.concat [|coeffs12;threshDelay|]

        let chooseTierIndexed tier parameters = 
            parameters |> Array.indexed
                       |> Array.choose (function 
                                         | i, ContinuousParameter(x,bounds,t) when t = tier -> Some (i, (x, bounds))
                                         | i, DiscreteParameter(x,bounds,t) when t = tier -> Some (i, (x, bounds)) 
                                         | _ -> None )

    type ContinuousObjectiveFunction = 
        | LeastSquares

    type ObjectiveFunction = 
        | ContinuousFunction of ContinuousObjectiveFunction

    type ContinuousOptimizationMethod = 
        | BFGS

    type DiscreteOptimizationMethod = 
        | IntegerMethod

    type OptimizationMethod = 
        | ContinuousMethod of ContinuousOptimizationMethod
        | DiscreteMethod of DiscreteOptimizationMethod

    type OptimizationProblem = 
        | Problem of OptimizationMethod * ObjectiveFunction * Params.ParameterTier * OptimizationProblem
        | End

    let predictionErrors array (T(name,(Graph(state,sk)),updateStrat)) = 
        let oneStepRollingForecastM truthPoint = Graph.TimeSerie.oneStepRollingForecastM name updateStrat sk truthPoint
        let pred, finalState = Graph.TimeSerie.fold1 oneStepRollingForecastM state array
        let error = UtilitiesSIMD.ArraySIMD.substract array pred
        (error,finalState)

    let inline continuousObjective lossFunction = 
        match lossFunction with
            | LeastSquares -> (fun error -> UtilitiesSIMD.ArraySIMD.mult error error |> UtilitiesSIMD.ArraySIMD.sum)

    let inline objective objectiveFunc = 
        match objectiveFunc with
            | ContinuousFunction(lossFunc) -> continuousObjective lossFunc

    let ofModel name =
        match name with
        | AR -> Problem(BFGS |> ContinuousMethod, LeastSquares |> ContinuousFunction, Params.Tier1, End)
        | MA -> Problem(BFGS |> ContinuousMethod, LeastSquares |> ContinuousFunction, Params.Tier1, End)
        | SETAR -> Problem(IntegerMethod |> DiscreteMethod, LeastSquares |> ContinuousFunction, Params.Tier2, 
                     Problem(BFGS |> ContinuousMethod, LeastSquares |> ContinuousFunction, Params.Tier1, End))

    let fit array (T(name,(Graph((GraphState(p,v,i,prev,c)),sk)),updateStrat)) = 
        let parameters = Params.ofModel name p
        let problem = ofModel name

        let errorFunction pa = predictionErrors array (T(name,(Graph((GraphState(pa,v,i,prev,c)),sk)),updateStrat))

(*        let solve prob = 
            match prob with
            | Problem(method, func, tier, sub) -> *)
        
        
        
        
        
    
    

    


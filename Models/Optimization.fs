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

    module Optimizer = 
        let J = NumericalJacobian()
        let limitGradient x = x |> Array.map (fun x -> if System.Double.IsNaN(x) then 1e10 else x) 
        let limitValueFunction x = if System.Double.IsInfinity(x) then 1e10 else x

        let vectorToArray (v:Vector<'T>) = v.ToArray() 
        let arrayToVector (array:'T[]) = Vector<'T>.Build.Dense array
        let limitedVectorFunc f = vectorToArray >> f >> limitValueFunction
        let jacobianOf (f:float[] -> float) x = J.Evaluate(f,x)
        let limitedVectorJacobian f = vectorToArray >> jacobianOf f >> limitGradient >> arrayToVector

        let BFGSOptimizer func (initGuess:float[]) =
            let objective = ObjectiveFunction.Gradient(System.Func<_,_> (limitedVectorFunc func), System.Func<_,_> (limitedVectorJacobian func))
            let solver = BfgsMinimizer(1e-5, 1e-5, 1e-5, 1000)
            let result = solver.FindMinimum(objective, Vector<float>.Build.Dense initGuess)
            result.MinimizingPoint.AsArray()
            
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
            
    let continuousMethod = function
        | BFGS -> Optimizer.BFGSOptimizer

    let discreteMethod = function
        | IntegerMethod -> Optimizer.BFGSOptimizer

    let optimizer method = 
        match method with
        | ContinuousMethod(m) -> continuousMethod m
        | DiscreteMethod(m) -> discreteMethod m

    let problemFor name =
        match name with
        | AR -> Problem(BFGS |> ContinuousMethod, LeastSquares |> ContinuousFunction, Params.Tier1, End)
        | MA -> Problem(BFGS |> ContinuousMethod, LeastSquares |> ContinuousFunction, Params.Tier1, End)
        | SETAR -> Problem(IntegerMethod |> DiscreteMethod, LeastSquares |> ContinuousFunction, Params.Tier2, 
                     Problem(BFGS |> ContinuousMethod, LeastSquares |> ContinuousFunction, Params.Tier1, End))

    let fit array (T(name,(Graph((GraphState(p,v,i,prev,c)),sk)),updateStrat)) = 
        let parameters = Params.ofModel name p
        let problem = problemFor name

        let errorFunction pa = predictionErrors array (T(name,(Graph((GraphState(pa,v,i,prev,c)),sk)),updateStrat))

        let chosenToOverallParameters (chosenParams:(int * 'a)[]) everyParams (chosenArray:float[]) = 
            let result = Array.copy everyParams
            chosenParams |> Array.iteri (fun i (j,_) -> result.[j] <- chosenArray.[i])
            result

        let errorFuncForChosenParams (chosenParams:(int * 'a)[]) everyParams = 
            let result = Array.copy everyParams
            let innerFunc chosenParamArray = 
                chosenParamArray |> Array.iteri ( fun i x -> result.[(fst (chosenParams.[i]))] <- x )
                errorFunction result |> fst
            innerFunc

        let initialGuess chosenParams (initParams:float[]) = 
            chosenParams |> Array.map (fun (i,_) -> initParams.[i])

        let rec solve prob initParams = 
            match prob with
            | Problem(method, loss, tier, sub) -> let chosenParams = Params.chooseTierIndexed tier parameters
                                                  let initGuess = initialGuess chosenParams initParams
                                                  let lossFunction = errorFuncForChosenParams chosenParams initParams >> objective loss
                                                  let fittedParams = optimizer method lossFunction initGuess
                                                                        |> chosenToOverallParameters chosenParams initParams
                                                  solve sub fittedParams
            | End -> initParams

        let fittedParameters = solve problem p
        (T(name,(Graph((GraphState(fittedParameters,v,i,prev,c)),sk)),updateStrat))
        
        
        
        
        
    
    

    


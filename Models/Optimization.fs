namespace Models

module Optimization = 
    open MathNet.Numerics.Optimization
    open MathNet.Numerics.Differentiation
    open MathNet.Numerics.LinearAlgebra

    module Bounds = 
        type ContinuousBound<'T> = ContinuousBound of 'T * 'T
        type DiscreteBound<'T> = DiscreteBound of 'T[]
        type B<'T> = ContinuousBound<'T> [] option * DiscreteBound<'T> [] option
 
        let ofModel name parameterArray = 
            match name with
            | AR -> let cBounds = parameterArray |> Array.map (fun _ -> (-1.0,1.0) |> ContinuousBound)
                    B(Some cBounds, None)
            | MA -> let cBounds = parameterArray |> Array.map (fun _ -> (-1.0,1.0) |> ContinuousBound)
                    B(Some cBounds, None)
            | SETAR -> let coeffs12 = Array.zeroCreate (parameterArray.Length - 2) |> Array.map (fun _ -> (-1.0,1.0) |> ContinuousBound)
                       let threshold = Array.init 61 (fun i -> float(i-30)*0.1) |> DiscreteBound
                       let delay = Array.init 11 (fun i -> float(i + 1)) |> DiscreteBound
                       B(Some coeffs12, Some [|threshold;delay|])
                                        
    module Parameter =
        type ContinuousParameterInfo<'T> = ContinuousParameterInfo of idx:int * Bounds.ContinuousBound<'T>
        type DiscreteParameterInfo<'T> = DiscreteParameterInfo of idx:int * Bounds.DiscreteBound<'T>
        type Info<'T> = ContinuousParameterInfo<'T> [] option * DiscreteParameterInfo<'T> [] option
        type Params<'T> = Params of 'T [] * Info<'T>

        let ofModel name array = 
            let arrayIndexed = Array.indexed array
            let cb, db = Bounds.ofModel name array
            let info = match name with 
                        | AR -> let cb = cb |> Option.get
                                let cp = arrayIndexed |> Array.map (fun (i,_) -> ContinuousParameterInfo(i,cb.[i]))
                                Info(Some cp, None)
                        | MA -> let cb = cb |> Option.get
                                let cp = arrayIndexed |> Array.map (fun (i,_) -> ContinuousParameterInfo(i,cb.[i]))
                                Info(Some cp, None)
                        | SETAR -> let cb = cb |> Option.get
                                   let db = db |> Option.get
                                   let coeffs12, threshDelay = arrayIndexed |> Array.splitAt (array.Length-2)
                                   let coeffs12 = coeffs12 |> Array.map (fun (i,_) -> ContinuousParameterInfo(i,cb.[i]))
                                   let threshDelay = threshDelay |> Array.map (fun (i,_) -> DiscreteParameterInfo(i,db.[i]))
                                   Info(Some coeffs12, Some threshDelay)
            Params(array, info)

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

        // let tryAll func (initGuess:float[]) = 
            
            
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

    type Problem<'T> = 
        | Classical of OptimizationMethod * ObjectiveFunction * Parameter.Params<'T>
        | Recursive of OptimizationMethod * ObjectiveFunction * Parameter.Params<'T> * Problem<'T>

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
        // | TryAll -> Optimizer.tryAll
        | IntegerMethod -> Optimizer.BFGSOptimizer

    let optimizer method = 
        match method with
        | ContinuousMethod(m) -> continuousMethod m
        | DiscreteMethod(m) -> discreteMethod m

    let problemFor name =
        match name with
        | AR -> Classical(BFGS |> ContinuousMethod, LeastSquares |> ContinuousFunction, Params.Continuous)
        | MA -> Classical(BFGS |> ContinuousMethod, LeastSquares |> ContinuousFunction, Params.Continuous)
        | SETAR -> Recursive(IntegerMethod |> DiscreteMethod, LeastSquares |> ContinuousFunction, Params.Discrete, 
                     Classical(BFGS |> ContinuousMethod, LeastSquares |> ContinuousFunction, Params.Continuous))

    let fit array (T(name,(Graph((GraphState(p,v,i,prev,c)),sk)),updateStrat)) = 
        let parametersInfo = ParameterInfo.ofModel name p
        let problem = problemFor name

        let errorFunction pa = predictionErrors array (T(name,(Graph((GraphState(pa,v,i,prev,c)),sk)),updateStrat)) 

        let partialToEntireParameters (partialParameters:(int * 'a)[]) everyParams (partialParametersArray:float[]) = 
            let result = Array.copy everyParams
            partialParameters |> Array.iteri (fun i (j,_) -> result.[j] <- partialParametersArray.[i])
            result

        let errorFuncForPartialParameters (partialParameters:(int * 'a)[]) f (initParams:float[]) = 
            let innerFunc partialParametersArray = 
                let (result:float[]) = f initParams
                partialParametersArray |> Array.iteri ( fun i x -> result.[(fst (partialParameters.[i]))] <- x )
                errorFunction result |> fst
            innerFunc

        let initialGuess partialParameters (initParams:float[]) = 
            partialParameters |> Array.map (fun (i,_) -> initParams.[i])

        let solve problem initParams = 
            match problem with
            | Classical(m,l,t) -> 

(*        let rec solve prob initParams = 
            match prob with
            | Problem(method, loss, tier, sub) -> let chosenParams = Params.chooseTierIndexed tier parameters
                                                  let initGuess = initialGuess chosenParams initParams
                                                  let lossFunction = errorFuncForChosenParams chosenParams (solve sub) initParams >> objective loss
                                                  optimizer method lossFunction initGuess
                                                          |> chosenToOverallParameters chosenParams initParams
                                                  
            | End -> initParams*)

        let fittedParameters = solve problem p
        (T(name,(Graph((GraphState(fittedParameters,v,i,prev,c)),sk)),updateStrat))
        
        
        
        
        
    
    

    


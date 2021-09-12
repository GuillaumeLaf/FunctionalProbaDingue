namespace Models

module Optimization = 
    open MathNet.Numerics.Optimization
    open MathNet.Numerics.Differentiation
    open MathNet.Numerics.LinearAlgebra

    module Bounds = 
        type ContinuousBound<'T> = ContinuousBound of 'T * 'T
        type DiscreteBound<'T> = DiscreteBound of 'T[]
        type B<'T> = ContinuousBound<'T> [] * DiscreteBound<'T> []
 
        // Create the bounds for each model.
        // When continuous, bounds are defined by a minimum and maximum value.
        // When discrete, bounds are defined by an array of possiblities.
        let ofModel name = 
            match name with
            | AR(order) | MA(order) -> let cBounds = Array.zeroCreate order |> Array.map (fun _ -> (-1.0,1.0) |> ContinuousBound)
                                       B(cBounds, [||])
(*            | SETAR -> let coeffs12 = Array.zeroCreate (parameterArray.Length - 2) |> Array.map (fun _ -> (-1.0,1.0) |> ContinuousBound)
                       let threshold = Array.init 10 (fun i -> float(i-30)*0.1) |> DiscreteBound
                       let delay = Array.init 3 (fun i -> float(i + 1)) |> DiscreteBound
                       B(coeffs12,[|threshold;delay|])*)

        // Extract the minimum and maximum value for several continous bounds.
        let continuousToTuple (bounds:ContinuousBound<'T>[]) = bounds |> Array.map (fun (ContinuousBound(mn,mx)) -> (mn,mx))

        // Extract in distinct arrays the minimum and maximum value of several continuous bounds.
        let continuousToSplittedArray (bounds:ContinuousBound<'T>[]) = 
            (continuousToTuple >> Utilities.extractFst) bounds, (continuousToTuple >> Utilities.extractSnd) bounds 

        let discreteToArray (bounds:DiscreteBound<'T>[]) = bounds |> Array.map (fun (DiscreteBound(possibilities)) -> possibilities)
                                        
    module Parameter =
        // Important Note : the current implementation of parameters only make the distinction between continuous and discrete parameters.
        // Future implementation should also encompass the possibility to have different tiers of continuous and discrete parameters. 
        type ParameterType = 
            | Continuous
            | Discrete

        // The 'idx' should correspond to a unique index in the parameter array of the model.
        type ContinuousParameterInfo<'T> = ContinuousParameterInfo of idx:int * Bounds.ContinuousBound<'T> 
        type DiscreteParameterInfo<'T> = DiscreteParameterInfo of idx:int * Bounds.DiscreteBound<'T>

        type Info<'T> = Info of ContinuousParameterInfo<'T> [] * DiscreteParameterInfo<'T> []
        type PartialInfo<'T> = PartialInfo of ContinuousParameterInfo<'T> [] * DiscreteParameterInfo<'T> []

        type Params<'T> = Params of 'T [] * Info<'T>
        type PartialParams<'T> = PartialParams of 'T[] * PartialInfo<'T>

        // Extract the array of parameters from a 'Params' type.
        let toArray (Params(array,_)) = array
        let toInfo (Params(_,info)) = info
        let partialToArray (PartialParams(array,_)) = array
        let partialToPartialInfo (PartialParams(_,partialInfo)) = partialInfo

        // Get a default array representing the disposition of parameters inside the model.
        let defaultParametersArrayForModel = function
            | AR(order) | MA(order) -> Array.zeroCreate order

        // Get the 'PartialInfo' about a particular parameter type (continuous or discrete) from an 'Info' type.
        let partialInfoFromInfo (Info(cinfo, dinfo)) = function
            | Continuous -> PartialInfo(cinfo, [||])
            | Discrete -> PartialInfo([||],dinfo)

        let partialInfoFromParams p t = (toInfo >> partialInfoFromInfo) p t

        // Get the index of continuous or discrete parameters from a 'PartialInfo'.
        let indexFromPartialInfo = function
            | PartialInfo(c,[||]) -> c |> Array.map (fun (ContinuousParameterInfo(i,_)) -> i)
            | PartialInfo([||],d) -> d |> Array.map (fun (DiscreteParameterInfo(i,_)) -> i)
            | PartialInfo(_,_) -> invalidArg "partialInfo" "Something went wront with getting the index of a partial info. Maybe trying to get a mix of continuous and discrete info."
   
        let boundsFrom (p:Params<'T>) = 
            let (Params(_,Info(cinfo,dinfo))) = p
            let cBounds = cinfo |> Array.map (fun (ContinuousParameterInfo(_,b)) -> b)
            let dBounds = dinfo |> Array.map (fun (DiscreteParameterInfo(_,b)) -> b)
            Bounds.B(cBounds, dBounds)

        let indexFromInfo info = partialInfoFromInfo info >> indexFromPartialInfo
        let indexFromParams p = (toInfo >> indexFromInfo) p

        let ofModel name = 
            let p = defaultParametersArrayForModel name
            let cb, db = Bounds.ofModel name
            let info = match name with 
                        | AR(order) | MA(order) -> let cp = Array.zeroCreate order |> Array.mapi (fun i _ -> ContinuousParameterInfo(i,cb.[i]))
                                                   Info(cp, [||])
(*                        | SETAR -> let coeffs12, threshDelay = arrayIndexed |> Array.splitAt (array.Length-2)
                                   let coeffs12 = coeffs12 |> Array.mapi (fun i _ -> ContinuousParameterInfo(i,cb.[i]))
                                   let threshDelay = threshDelay |> Array.mapi (fun i _ -> DiscreteParameterInfo(i+coeffs12.Length,db.[i]))
                                   Info(coeffs12, threshDelay)*)
            Params(p, info)

        // Get the complement type of a given one.
        let notType t = function
            | Continuous -> Discrete
            | Discrete -> Continuous

        let subArrayFromParams ((Params(array:'T[],info))) = function 
            | Continuous -> let cidx = indexFromParams ((Params(array,info))) Continuous
                            [|for i in cidx do array.[i]|]
            | Discrete -> let didx = indexFromParams ((Params(array,info))) Discrete
                          [|for i in didx do array.[i]|]

        let PartialParamsFromParams p t = (subArrayFromParams p t, partialInfoFromParams p t) |> PartialParams

        // Split the given 'Params' into 'PartialParams'.
        // The first one contains the continuous parameters (along with its info).
        // The second one has the discrete parameters (along with its info).
        let split p = PartialParamsFromParams p Continuous, PartialParamsFromParams p Discrete

        // Extract the 'PartialParams' of a 'Params' type given the parameter type desired.
        let extractPartial (p:Params<'T>) = function 
            | Continuous -> p |> split |> fst
            | Discrete -> p |> split |> snd

        // Create a 'Params' type from two 'PartialParams'.
        // Note that the 'Params' should contain all the parameters for a model. 
        // Only 'PartialParams' should have some and not all parameters of a model.
        // The first 'PartialParams' should correspond to the continuous parameters.
        // To avoid switching things up, I should sort the array by the parameter's index.
        let group (PartialParams(array1,(PartialInfo(cp1,_)))) (PartialParams(array2,(PartialInfo(_,dp2)))) = 
           (Array.concat [|array1;array2|],Info(cp1,dp2)) |> Params

        // replace in a 'Params' type the corresponding 'PartialParams'
        // Note that the 'PartialParams' should correspond either to continuous or discrete parameters but not a mix. 
        let replaceIn (p:Params<'T>) (PartialParams(partialArray, partialInfo)) =
            let cSplit, dSplit = p |> split
            match partialInfo with
            | PartialInfo(c, [||]) -> group (PartialParams(partialArray, partialInfo)) dSplit
            | PartialInfo([||], d) -> group cSplit (PartialParams(partialArray, partialInfo))
            | _ -> invalidArg "partialInfo" "Something went wrong with replacing partial parameters into parameters."

        // Get partial parameter array from a 'Params' for a given parameter type (continuous or discrete).
        let partialArray p = extractPartial p >> partialToArray

    module Optimizer = 
        type OptimizerType = 
            | Continuous of ((float[] -> float) -> float[] -> Bounds.ContinuousBound<float>[] -> float[])
            | Discrete of ((float[] -> float) -> float[] -> Bounds.DiscreteBound<float>[] -> float[])
    
        let J = NumericalJacobian()
        let limitGradient x = x |> Array.map (fun x -> if System.Double.IsNaN(x) then 1e10 else x) 
        let limitValueFunction x = if System.Double.IsInfinity(x) then 1e10 else x

        let vectorToArray (v:Vector<'T>) = v.ToArray() 
        let arrayToVector (array:'T[]) = Vector<'T>.Build.Dense array

        // Transform a function to accept vectors as input and limit its value if it explodes.
        let limitedVectorFunc f = vectorToArray >> f >> limitValueFunction

        // Get the jacobian of a function at a point 'x'
        let jacobianOf (f:float[] -> float) x = J.Evaluate(f,x)

        // Transform the jacobian of a function to accept vectors as input and limit its value if it explodes.
        let limitedVectorJacobian f = vectorToArray >> jacobianOf f >> limitGradient >> arrayToVector

        // Output the optimal parameters as an array
        let BFGSOptimizer func (initGuess:float[]) (bounds:Bounds.ContinuousBound<float>[]) =
            let boundsSplit = Bounds.continuousToSplittedArray bounds
            //let objective = ObjectiveFunction.Gradient(System.Func<_,_> (limitedVectorFunc func), System.Func<_,_> (limitedVectorJacobian func))
            let objective = ObjectiveFunction.Value(System.Func<_,_> (limitedVectorFunc func))
            let grad = ObjectiveFunctions.ForwardDifferenceGradientObjectiveFunction(objective, arrayToVector (fst boundsSplit), arrayToVector (snd boundsSplit))
            let solver = BfgsMinimizer(1e-5, 1e-5, 1e-5, 1000)
            let result = solver.FindMinimum(grad, Vector<float>.Build.Dense initGuess)
            result.MinimizingPoint.AsArray()

        // Output the optimal parameters as an array
        let BruteForce2 func (initGuess:float[]) (bounds:Bounds.DiscreteBound<float>[]) = 
            let boundsArray = bounds |> Bounds.discreteToArray
            let cartProd = boundsArray |> Utilities.cartesianProductArray
            let results = cartProd |> Array.Parallel.map (fun arr -> func arr)
            printfn "%A" results
            results |> Array.zip cartProd
                    |> Array.minBy (fun (_,r) -> r)
                    |> fst
            
    type ContinuousObjectiveFunction = 
        | LeastSquares

    type ObjectiveFunction = 
        | ContinuousFunction of ContinuousObjectiveFunction

    type ContinuousOptimizationMethod = 
        | BFGS

    type DiscreteOptimizationMethod = 
        | BruteForce
        | IntegerMethod

    type OptimizationMethod = 
        | ContinuousMethod of ContinuousOptimizationMethod
        | DiscreteMethod of DiscreteOptimizationMethod

    type Problem<'T> = 
        | Classical of OptimizationMethod * ObjectiveFunction * Parameter.ParameterType
        | Recursive of OptimizationMethod * ObjectiveFunction * Parameter.ParameterType * Problem<'T>

(*    let predictionErrors array (T(name,(Graph(state,sk)),updateStrat)) = 
        let oneStepRollingForecastM truthPoint = Graph.TimeSerie.oneStepRollingForecastM name updateStrat sk truthPoint
        let pred, finalState = Graph.TimeSerie.fold1 oneStepRollingForecastM state array
        let error = UtilitiesSIMD.ArraySIMD.substract array pred
        (error,finalState)*)

    let inline continuousObjective lossFunction = 
        match lossFunction with  
            | LeastSquares -> (fun error -> UtilitiesSIMD.ArraySIMD.mult error error |> UtilitiesSIMD.ArraySIMD.sum)

    let inline objective objectiveFunc = 
        match objectiveFunc with
            | ContinuousFunction(lossFunc) -> continuousObjective lossFunc
            
    let continuousMethod = function
        | BFGS -> Optimizer.BFGSOptimizer |> Optimizer.Continuous

    let discreteMethod = function
        | BruteForce -> Optimizer.BruteForce2 |> Optimizer.Discrete
        | IntegerMethod -> Optimizer.BFGSOptimizer |> Optimizer.Continuous

    let optimizer method = 
        match method with
        | ContinuousMethod(m) -> continuousMethod m
        | DiscreteMethod(m) -> discreteMethod m

    let problemFor name =
        match name with
        | AR(_) | MA(_) -> Classical(BFGS |> ContinuousMethod, LeastSquares |> ContinuousFunction, Parameter.Continuous)
        (*| SETAR -> Recursive(BruteForce |> DiscreteMethod, LeastSquares |> ContinuousFunction, Parameter.Discrete, 
                     Classical(BFGS |> ContinuousMethod, LeastSquares |> ContinuousFunction, Parameter.Continuous))*)

    let fit name array = 
        let parameters = Parameter.ofModel name
        let cbounds,dbounds = Parameter.boundsFrom parameters
        let problem = problemFor name

        let initStateTS = TimeSeries.UnivariateTimeSeries.defaultStateFrom array
        let skM = MonadicGraph.modelM (Fitting(name))
        let updteVarM = GraphTimeSeries.updateVariablesForFittingM name
        let fittingM = GraphTimeSeries.fitOnceM updteVarM skM

        let (MonadicGraph.State(_,v,i)) = MonadicGraph.defaultStateForFitting name

        let errorFunction pa = 
            let (TimeSeries.UnivariateTimeSeries.State(_,_,error)) = GraphTimeSeries.foldRun fittingM initStateTS (MonadicGraph.State(pa,v,i)) |> fst
            error |> Array.map (fun x -> Option.defaultValue 0.0 x)

        let partialToParameter partialType (p:Parameter.Params<'T>) partialArray = 
            let partialInfo = Parameter.partialInfoFromParams p partialType
            Parameter.replaceIn p (Parameter.PartialParams(partialArray,partialInfo))

        let run loss initialGuess opt = 
            match opt with
            | Optimizer.Continuous(innerFunc) -> innerFunc loss initialGuess cbounds
            | Optimizer.Discrete(innerFunc) -> innerFunc loss initialGuess dbounds

        let rec solve problem initParams = 
            match problem with
            | Classical(m,l,t) -> let initGuess = t |> Parameter.partialArray initParams
                                  let lossFunction = partialToParameter t initParams >> Parameter.toArray >> errorFunction>> objective l
                                  let result = run lossFunction initGuess (optimizer m) |> partialToParameter t initParams
                                  result 

            | Recursive(m,l,t,sub) -> let initGuess = t |> Parameter.partialArray initParams
                                      let lossFunction = partialToParameter t initParams >> (solve sub) >> Parameter.toArray >> errorFunction >> objective l
                                      run lossFunction initGuess (optimizer m)
                                        |> partialToParameter t initParams

        let (Parameter.Params(fittedParameters, _)) = solve problem parameters
        MonadicGraph.State(fittedParameters,v,i)
        
        
        
        
        
    
    

    


namespace Models

module Optimization = 
    open MathNet.Numerics.Optimization
    open MathNet.Numerics.Differentiation
    open MathNet.Numerics.LinearAlgebra

    module Bounds = 
        type ContinuousBound<'T> = ContinuousBound of 'T * 'T
        type DiscreteBound<'T> = DiscreteBound of 'T[]
        type B<'T> = ContinuousBound<'T> [] * DiscreteBound<'T> []
 
        let ofModel name parameterArray = 
            match name with
            | AR -> let cBounds = parameterArray |> Array.map (fun _ -> (-1.0,1.0) |> ContinuousBound)
                    B(cBounds, [||])
            | MA -> let cBounds = parameterArray |> Array.map (fun _ -> (-1.0,1.0) |> ContinuousBound)
                    B(cBounds, [||])
            | SETAR -> let coeffs12 = Array.zeroCreate (parameterArray.Length - 2) |> Array.map (fun _ -> (-1.0,1.0) |> ContinuousBound)
                       let threshold = Array.init 61 (fun i -> float(i-30)*0.1) |> DiscreteBound
                       let delay = Array.init 11 (fun i -> float(i + 1)) |> DiscreteBound
                       B(coeffs12,[|threshold;delay|])
                                        
    module Parameter =
        type ParameterType = 
            | Continuous
            | Discrete
        type ContinuousParameterInfo<'T> = ContinuousParameterInfo of idx:int * Bounds.ContinuousBound<'T>
        type DiscreteParameterInfo<'T> = DiscreteParameterInfo of idx:int * Bounds.DiscreteBound<'T>

        type Info<'T> = Info of ContinuousParameterInfo<'T> [] * DiscreteParameterInfo<'T> []
        type PartialInfo<'T> = PartialInfo of ContinuousParameterInfo<'T> [] * DiscreteParameterInfo<'T> []

        type Params<'T> = Params of 'T [] * Info<'T>
        type PartialParams<'T> = PartialParams of 'T[] * PartialInfo<'T>

        let indexInfo t info = 
            let (Info(cinfo, dinfo)) = info
            match t with 
            | Continuous -> cinfo |> Array.map (fun (ContinuousParameterInfo(i,_)) -> i)  
            | Discrete -> dinfo |> Array.map (fun (DiscreteParameterInfo(i,_)) -> i)         
             
        let ofModel name array = 
            let arrayIndexed = Array.indexed array
            let cb, db = Bounds.ofModel name array
            let info = match name with 
                        | AR -> let cp = arrayIndexed |> Array.map (fun (i,_) -> ContinuousParameterInfo(i,cb.[i]))
                                Info(cp, [||])
                        | MA -> let cp = arrayIndexed |> Array.map (fun (i,_) -> ContinuousParameterInfo(i,cb.[i]))
                                Info(cp, [||])
                        | SETAR -> let coeffs12, threshDelay = arrayIndexed |> Array.splitAt (array.Length-2)
                                   let coeffs12 = coeffs12 |> Array.map (fun (i,_) -> ContinuousParameterInfo(i,cb.[i]))
                                   let threshDelay = threshDelay |> Array.map (fun (i,_) -> DiscreteParameterInfo(i,db.[i]))
                                   Info(coeffs12, threshDelay)
            Params(array, info)

        let toArray (Params(array,_)) = array

        let split (p:Params<'T>) = 
            let (Params(array,info)) = p
            let (Info(cinfo,dinfo)) = info
            let cidx = indexInfo Continuous info
            let didx = indexInfo Discrete info
            let cSubArray = [|for i in cidx do array.[i]|]
            let dSubArray = [|for i in didx do array.[i]|]
            PartialParams(cSubArray,PartialInfo(cinfo,[||])),PartialParams(dSubArray,PartialInfo([||],dinfo))

        let extractPartial t (p:Params<'T>) = 
            let cSplit, dSplit = p |> split
            match t with
            | Continuous -> cSplit
            | Discrete -> dSplit

        let group (PartialParams(array1,(PartialInfo(cp1,dp1)))) (PartialParams(array2,(PartialInfo(cp2,dp2)))) = 
            let array = Array.concat [|array1;array2|]
            let cp = Array.concat [|cp1;cp2|]
            let dp = Array.concat [|dp1;dp2|]
            Params(array,Info(cp,dp))

        let replaceIn (p:Params<'T>) (PartialParams(partialArray, partialInfo)) =
            let cSplit, dSplit = p |> split
            match partialInfo with
            | PartialInfo(c, [||]) -> group (PartialParams(partialArray, partialInfo)) dSplit
            | PartialInfo([||], d) -> group cSplit (PartialParams(partialArray, partialInfo))
            | _ -> invalidArg "partialInfo" "Something went wrong with replacing partial parameters into parameters."

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
        | Classical of OptimizationMethod * ObjectiveFunction * Parameter.ParameterType
        | Recursive of OptimizationMethod * ObjectiveFunction * Parameter.ParameterType * Problem<'T>

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
        // | BruteForce -> Optimizer.BruteForce
        | IntegerMethod -> Optimizer.BFGSOptimizer

    let optimizer method = 
        match method with
        | ContinuousMethod(m) -> continuousMethod m
        | DiscreteMethod(m) -> discreteMethod m

    let problemFor name =
        match name with
        | AR -> Classical(BFGS |> ContinuousMethod, LeastSquares |> ContinuousFunction, Parameter.Continuous)
        | MA -> Classical(BFGS |> ContinuousMethod, LeastSquares |> ContinuousFunction, Parameter.Continuous)
        | SETAR -> Recursive(IntegerMethod |> DiscreteMethod, LeastSquares |> ContinuousFunction, Parameter.Discrete, 
                     Classical(BFGS |> ContinuousMethod, LeastSquares |> ContinuousFunction, Parameter.Continuous))

    let fit array (T(name,(Graph((GraphState(p,v,i,prev,c)),sk)),updateStrat)) = 
        let parameters = Parameter.ofModel name p
        let problem = problemFor name

        let errorFunction pa = predictionErrors array (T(name,(Graph((GraphState(pa,v,i,prev,c)),sk)),updateStrat)) |> fst

        let partialToParameter partialType (p:Parameter.Params<'T>) partialArray = 
            let (Parameter.PartialParams(_, partialInfo)) = Parameter.extractPartial partialType p
            Parameter.replaceIn p (Parameter.PartialParams(partialArray,partialInfo))

        let initialGuessFrom partialType (p:Parameter.Params<'T>) = 
            let (Parameter.PartialParams(carray,_)), (Parameter.PartialParams(darray,_)) = p |> Parameter.split
            match partialType with
            | Parameter.Continuous -> carray
            | Parameter.Discrete -> darray

        let rec solve problem initParams = 
            match problem with
            | Classical(m,l,t) -> let initGuess = initParams |> initialGuessFrom t
                                  let lossFunction = partialToParameter t initParams >> Parameter.toArray >> errorFunction>> objective l
                                  optimizer m lossFunction initGuess
                                    |> partialToParameter t initParams

            | Recursive(m,l,t,sub) -> let initGuess = initParams |> initialGuessFrom t
                                      let recursiveFunc = (fun x -> solve sub (partialToParameter t initParams x))
                                      let lossFunction = partialToParameter t initParams >> Parameter.toArray >> errorFunction >> objective l
                                      optimizer m lossFunction initGuess
                                        |> partialToParameter t initParams

        let (Parameter.Params(fittedParameters, _)) = solve problem parameters
        (T(name,(Graph((GraphState(fittedParameters,v,i,prev,c)),sk)),updateStrat))
        
        
        
        
        
    
    

    


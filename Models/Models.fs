namespace Models

module Model =

    // https://lorgonblog.wordpress.com/2008/04/05/catamorphisms-part-one/
    open MathNet.Numerics.Optimization
    open MathNet.Numerics.Differentiation
    open MathNet.Numerics.LinearAlgebra
    open Distributions

    let inline ( +. ) (N1:Skeleton) (N2:Skeleton) = Node(Addition, N1, N2)
    let inline ( *. ) (N1:Skeleton) (N2:Skeleton) = Node(Multiplication, N1, N2)
    let inline ( -. ) (N1:Skeleton) (N2:Skeleton) = Node(Substraction, N1, N2)
    let inline ( <. ) (N1:Skeleton) (N2:Skeleton) = Node(LessThan, N1, N2)

    let (<*>) = Monad.apply
    let (<!>) = Monad.map
            
    let updateStrategyMA order = function
        | 0 -> Some (Innovation,0)
        | i when i < order -> Some (Variable,i-1)
        | _ -> None

    let updateStrategyAR order = function
        | 0 -> Some (PreviousResult,0)
        | i when i < order -> Some (Variable,i-1)
        | _ -> None

    let rec buildSkeleton (ModelType(_,parameters)) = 
        match parameters with
        | MAparams(coeffs) -> Array.zeroCreate coeffs.Length |> Array.mapi (fun i _ -> Leaf(Parameter,None,i,None))
                                                             |> Array.mapi (fun i p -> Leaf(Variable,None,i,updateStrategyMA coeffs.Length i) |> ( *. ) p)
                                                             |> Array.reduce ( +. )
                                                             |> ( +. ) (Leaf(Innovation,None,0,None))

        | ARparams(coeffs) -> Array.zeroCreate coeffs.Length |> Array.mapi (fun i _ -> Leaf(Parameter,None,i,None))
                                                                                |> Array.mapi (fun i p -> Leaf(Variable,None,i,updateStrategyAR coeffs.Length i) |> ( *. ) p)
                                                                                |> Array.reduce ( +. )
                                                                                |> ( +. ) (Leaf(Innovation,None,0,None))

        | SETARparams(coeffs1,coeffs2,threshold,delay) -> let ar1 = buildSkeleton (ModelType(AR,ARparams(coeffs1)))
                                                          let ar2 = buildSkeleton (ModelType(AR,ARparams(coeffs2)))
                                                          let maxOrderIsModel1 = if coeffs1.Length >= coeffs2.Length then true else false
                                                          let maxDelay = max coeffs1.Length coeffs2.Length
                                                          let mixingVariable = match delay with
                                                                                | 0 -> invalidArg "delay" "Delay cannot be less than 1."
                                                                                | 1 -> Leaf(Variable,None,0,Some (PreviousResult, 0))
                                                                                | x when maxOrderIsModel1 && x <= coeffs1.Length -> Leaf(Variable,None,0,Some (Variable, -coeffs1.Length-coeffs2.Length+int(delay)-1))
                                                                                | x when (not maxOrderIsModel1) && x > coeffs1.Length -> Leaf(Variable,None,0,Some (Variable, -coeffs2.Length+int(delay)-1))
                                                                                | x when x > maxDelay -> invalidArg "delay" "Delay cannot exceed the maximum Order of the two AR models."
                                                                                | _ -> invalidArg "delay" "Something went wrong with the delay parameter..."
                                                          let mixingCondition = (Leaf(Parameter,None,0,None) <. mixingVariable)
                                                          Node.mixture mixingCondition ar1 ar2
                                                          |> Graph.Skeleton.groupInnovations [|0;0|]

    let buildDefaultGraphStateFrom skeleton = 
        let constantsValues = Graph.Skeleton.valueOfConstants skeleton
        let graphStateFromArrays a = 
            a |> Array.map (fun x -> match fst x with
                                      | Innovation -> 2,snd x
                                      | Parameter -> 0,snd x
                                      | Variable -> 1,snd x 
                                      | PreviousResult -> 3,snd x
                                      | Constant -> 4,snd x)
              |> Array.sortBy (fun x -> fst x)
              |> Array.map (fun x -> snd x)
              |> (fun array -> GraphState(array.[0],array.[1],array.[2],array.[3],constantsValues))
        skeleton |> Graph.Skeleton.countNodeByInput
                 |> InputCounter.fromArray
                 |> InputCounter.AddOneIfZero
                 |> InputCounter.toArray
                 |> Array.map (fun x -> (fst x, Array.zeroCreate (snd x)))
                 |> graphStateFromArrays

    let setParameters parametersArray (Graph((GraphState(_,v,i,prev,c)),skeleton)) = Graph(GraphState(parametersArray,v,i,prev,c),skeleton)
    
    let createParametersArray p = 
        match p with
        | MAparams(coeffs) | ARparams(coeffs) -> coeffs
        | SETARparams(c1,c2,thresh,delay) -> Array.concat [|c1;c2;[|thresh|];[|float(delay)|]|]

    let build name parameters =
        let modeltype = ModelType(name, parameters) // check if the parameters correspond to the model name.
        let skeleton = buildSkeleton modeltype
        let defaultGraphState = buildDefaultGraphStateFrom skeleton
        let graph = Graph(defaultGraphState, skeleton) |> setParameters (createParametersArray parameters)
        let updateStrategy = Graph.getUpdatingStrategy graph |> UpdateVariableStrategy
        T(name, graph, updateStrategy)

    let sample (n:int) (T(_,(Graph(state,sk)),updateStrat)) = 
        let distr = Distributions.Norm(0.0, 1.0) |> Distributions.create
        let sampleOnceM = 
            let innerFunc fwdPass variables innovations = fwdPass
            innerFunc <!> Graph.TimeSerie.forwardPassM sk
                      <*> Graph.TimeSerie.reorganizeVariablesM updateStrat
                      <*> Graph.TimeSerie.randomInnovationsM distr
        Graph.TimeSerie.fold sampleOnceM state (Array.zeroCreate n) |> fst

    let conditionalExpectation steps (T(_,(Graph(state,sk)),updateStrat)) = 
        let conditionalExpectationM = Graph.TimeSerie.conditionalExpectationM updateStrat sk
        Graph.TimeSerie.fold conditionalExpectationM state (Array.zeroCreate steps) |> fst

    let oneStepRollingForecast array (T(name,(Graph(state,sk)),updateStrat)) = 
        let oneStepRollingForecastM truthPoint = Graph.TimeSerie.oneStepRollingForecastM name updateStrat sk truthPoint
        Graph.TimeSerie.fold1 oneStepRollingForecastM state array |> fst
                             
(*    let rollingConditionalExpectation steps (array:float array) (T(name,(Graph(state,sk)),updateStrat)) = 
        let rollingConditionalExpectationM = Graph.TimeSerie.rollingConditionalExpectationM steps updateStrat sk
        let result = Graph.TimeSerie.fold rollingConditionalExpectationM state array |> fst
        Array.concat [|Array.zeroCreate (steps-1);result|] // to get same length as 'array'.*)
               
    let fit (array:float array) (T(name,graph,updateStrat)) = 
        let len = array.Length
        let (Graph(GraphState(initParam,initVariables,initInnov,initPrev,initConstants),initSkeleton)) = graph

        let leastSquareFunction pa = 
            let OneStepRollingForecastM truthPoint = 
                let innerFunc fwdPass variables innovations = fwdPass
                innerFunc <!> Graph.TimeSerie.forwardPassM initSkeleton
                          <*> Graph.TimeSerie.reorganizeVariableWithTruthM name updateStrat truthPoint
                          <*> Graph.TimeSerie.inactiveInnovationsM
            Graph.TimeSerie.fold1 OneStepRollingForecastM
                                  (GraphState(pa,initVariables,initInnov,initPrev,initConstants))
                                  array
                              |> fst
                              |> Array.map2 (fun x pred -> (x - pred) * (x - pred)) array
                              |> Array.reduce (+)
            
        // When optimizing the tested model may not be stationary, we normalized the explosive behavior to continue optimizing.
        let limitGradient x = x |> Array.map (fun x -> if System.Double.IsNaN(x) then 1e10 else x) 
        let limitValueFunction x = if System.Double.IsInfinity(x) then 1e10 else x
        
        let leastSquareFunctionVector (v:Vector<float>) = v.ToArray() |> (leastSquareFunction >> limitValueFunction)
        let j = NumericalJacobian()
        let gradientLS x = j.Evaluate(leastSquareFunction, x)
        let gradientVector (v:Vector<float>) = v.ToArray() |> (gradientLS >> limitGradient >> Vector<float>.Build.Dense)

        let obj = ObjectiveFunction.Gradient(System.Func<_,_> leastSquareFunctionVector, System.Func<_,_> gradientVector)
        let solver = BfgsMinimizer(1e-5, 1e-5, 1e-5, 1000)
        let result = solver.FindMinimum(obj, Vector<float>.Build.Dense initParam)
        let fittedParameters = result.MinimizingPoint.AsArray()

        T(name, Graph(GraphState(fittedParameters,initVariables,initInnov,initPrev,initConstants),initSkeleton),updateStrat)



                       

       
      
    







    



    

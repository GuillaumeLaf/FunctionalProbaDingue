namespace Models

module Model =

    // https://lorgonblog.wordpress.com/2008/04/05/catamorphisms-part-one/
    open MathNet.Numerics.Optimization
    open MathNet.Numerics.Differentiation
    open MathNet.Numerics.LinearAlgebra
    open Distributions

(*    let conditionalExpectation steps (T(_,(Graph(state,sk)),updateStrat)) = 
        let conditionalExpectationM = Graph.TimeSerie.conditionalExpectationM updateStrat sk
        Graph.TimeSerie.fold conditionalExpectationM state (Array.zeroCreate steps) |> fst

    let oneStepRollingForecast array (T(name,(Graph(state,sk)),updateStrat)) = 
        let oneStepRollingForecastM truthPoint = Graph.TimeSerie.oneStepRollingForecastM name updateStrat sk truthPoint
        Graph.TimeSerie.fold1 oneStepRollingForecastM state array |> fst
                             
*)(*    let rollingConditionalExpectation steps (array:float array) (T(name,(Graph(state,sk)),updateStrat)) = 
        let rollingConditionalExpectationM = Graph.TimeSerie.rollingConditionalExpectationM steps updateStrat sk
        let result = Graph.TimeSerie.fold rollingConditionalExpectationM state array |> fst
        Array.concat [|Array.zeroCreate (steps-1);result|] // to get same length as 'array'.*)(*
               
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

        T(name, Graph(GraphState(fittedParameters,initVariables,initInnov,initPrev,initConstants),initSkeleton),updateStrat)*)



                       

       
      
    







    



    

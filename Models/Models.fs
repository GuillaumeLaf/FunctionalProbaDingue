﻿namespace Models

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
            
    let updateStrategyMA order = function
        | 0 -> Some (Innovation,0)
        | i when i < order -> Some (Variable,i-1)
        | _ -> None

    let updateStrategyAR order = function
        | 0 -> Some (PreviousResult,0)
        | i when i < order -> Some (Variable,i-1)
        | _ -> None

    let updateStrategySETAR order1 order2 delay = function
        | 0 -> Some (PreviousResult,0)
        | i when i = order1 -> Some (PreviousResult,0)
        | i when i < order1+order2 -> Some (Variable,i-1)
        | i when i = order1+order2 && int(delay) = 1 -> Some (PreviousResult,0)
        | i when i = order1+order2 && int(delay) <= order2 && int(delay) >= 1 -> Some (Variable,order1+int(delay)-1)
        | i when i = order1+order2 && (int(delay) >= order2 || int(delay) <= 1) -> invalidArg "delay" "Delay must be between 1 and order2." // find a way to better restrict parameters.
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

        | SETARparams(coeffs1,coeffs2,threshold,delay) -> let ar = buildSkeleton (ModelType(AR,ARparams(coeffs1)))
                                                          let mixingVariable = match delay with
                                                                                | 0 -> invalidArg "delay" "Delay cannot be less than 1."
                                                                                | 1 -> Leaf(Variable,None,coeffs1.Length+coeffs2.Length,Some (PreviousResult, 0))
                                                                                | x when x <= coeffs1.Length -> Leaf(Variable,None,coeffs1.Length+coeffs2.Length,Some (Variable, -coeffs1.Length-coeffs2.Length+int(delay)-1))
                                                                                | x when x > coeffs1.Length -> invalidArg "delay" "Delay cannot exceed the order of the first AR model."
                                                                                | _ -> invalidArg "delay" "Something when wrong with the delay parameter..."
                                                          let mixingCondition = (Leaf(Parameter,None,coeffs1.Length+coeffs2.Length,None) <. mixingVariable)
                                                          Node.mixture mixingCondition ar ar

(*        | SETARparams(coeffs1,coeffs2,threshold,delay) -> let ar1 = Array.zeroCreate coeffs1.Length |> Array.mapi (fun i _ -> Leaf(Parameter,None,i,None))
                                                                                                    |> Array.mapi (fun i p -> Leaf(Variable,None,i,updateStrategyAR coeffs.Length i) |> ( *. ) p)
                                                                                                    |> Array.reduce ( +. )
                                                                                                    |> ( +. ) (Leaf(Innovation,None,0,None))

                                                          let ar2 = Array.zeroCreate coeffs2.Length |> Array.mapi (fun i _ -> Leaf(Parameter,None,i+coeffs1.Length))
                                                                                                    |> Array.mapi (fun i p -> Leaf(Variable,None,i+coeffs1.Length) |> ( *. ) p)
                                                                                                    |> Array.reduce ( +. )
                                                                                                    |> ( +. ) (Leaf(Innovation,None,0))
                                                          
                                                          let cond = (Leaf(Parameter,None,coeffs1.Length+coeffs2.Length) <. Leaf(Variable,None,coeffs1.Length+coeffs2.Length))
                                                          (cond *. ar1) +. ((Leaf(Constant,Some 1.0,0) -. cond) *. ar2) *)

    let buildDefaultGraphStateFrom skeleton = 
        let constantsValues = Graph.getValueOfConstants skeleton
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
        skeleton |> Graph.countNodeByInputFromSK 
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

    let updateVariables (UpdateVariableStrategy(pullfrom)) (GraphState(p,v,innov,prevResult,constant)) = 
        Array.zeroCreate v.Length |> Array.mapi (fun i _ -> match pullfrom.[i] with  
                                                            | Some (Innovation, idx) -> innov.[idx]
                                                            | Some (Parameter, idx) -> p.[idx]
                                                            | Some (Variable, idx) -> v.[idx]
                                                            | Some (PreviousResult, idx) -> prevResult.[idx]  // Make sure the idx is a possible index of array.
                                                            | Some (Constant,idx) -> constant.[idx] 
                                                            | None -> v.[i])
    
    let foldGraphTimeSeries nextParamF nextVarF nextGraphF nextInnovF (array:float array) graph = 
        let (Graph(state,sk)) = graph
        let updateGraphState state x = 
            let currentResult = Graph.forwardPass (Graph(state,sk))
            let (GraphState(_,_,_,_,c)) = state
            GraphState(nextParamF x state, state |> nextGraphF x currentResult |>  nextVarF x, nextInnovF x state, [|currentResult|],c)
        Array.scan updateGraphState state array |> Array.skip 1

    let sample (n:int) (T(_,graph,updateStrat)) = 
        let result = Array.zeroCreate n
        let distr = Distributions.Norm(0.0, 1.0) |> create
        foldGraphTimeSeries (fun _ (GraphState(p,_,_,_,_)) -> p)
                            (fun _ state -> updateVariables updateStrat state)
                            (fun _ currentResult (GraphState(p,v,i,_,c)) -> GraphState(p,v,i,[|currentResult|],c))
                            (fun _ (GraphState(_,_,innov,_,_)) -> [| for i in 0..innov.Length-1 do distr |> Distributions.sample |])
                            result
                            graph
                |> Array.map (fun (GraphState(_,_,_,prev,_)) -> prev.[0])

    let fit (array:float array) (T(name,graph,updateStrat)) = 
        let len = array.Length
        let (Graph(GraphState(initParam,initVariables,initInnov,initPrev,initConstants),initSkeleton)) = graph

        let updateGraphWithTruth (dataPoint:float) (expectation:float) (GraphState(p,v,i,prev,c)) = 
            match name with
            | MA -> GraphState(p,v,[|dataPoint - expectation|],prev,c)
            | AR -> GraphState(p,v,i,[|dataPoint|],c)
            | SETAR -> GraphState(p,v,i,[|dataPoint|],c)

        let mutable finalVariables = initVariables

        let leastSquareFunction pa = 
            let tmp = foldGraphTimeSeries (fun _ (GraphState(p,_,_,_,_)) -> p)
                                          (fun _ g -> updateVariables updateStrat g)
                                          (fun dataPoint expect g -> updateGraphWithTruth dataPoint expect g)
                                          (fun _ (GraphState(_,_,innov,_,_)) -> Array.zeroCreate innov.Length)
                                          array
                                          (Graph(GraphState(pa,initVariables,initInnov,initPrev,initConstants),initSkeleton))
            let (GraphState(_,v,_,_,_)) = Array.last tmp
            finalVariables <- v
            tmp |> Array.map2 (fun x (GraphState(_,_,_,prev,_)) -> (x - prev.[0]) * (x - prev.[0])) array
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
        T(name, Graph(GraphState(fittedParameters,finalVariables,initInnov,initPrev,initConstants),initSkeleton),updateStrat)

    let conditionalExpectation steps (T(name,graph,updateStrat)) = 
        foldGraphTimeSeries (fun _ (GraphState(p,_,_,_,_)) -> p)
                            (fun _ g -> updateVariables updateStrat g)
                            (fun _ currentResult (GraphState(p,v,i,_,c)) -> GraphState(p,v,i,[|currentResult|],c))
                            (fun _ (GraphState(_,_,innov,_,_)) -> Array.zeroCreate innov.Length)
                            (Array.zeroCreate steps)
                            graph
                |> Array.map (fun (GraphState(_,_,_,prev,_)) -> prev.[0])


                       

       
      
    







    



    

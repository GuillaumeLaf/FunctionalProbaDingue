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
                      
    let buildSkeleton (ModelType(_,parameters)) = 
        match parameters with
        | MAparams(coeffs) | ARparams(coeffs) -> Array.zeroCreate coeffs.Length |> Array.mapi (fun i _ -> Leaf(Parameter,None,i))
                                                                                |> Array.mapi (fun i p -> Leaf(Variable,None,i) |> ( *. ) p)
                                                                                |> Array.reduce ( +. )
                                                                                |> ( +. ) (Leaf(Innovation,None,0))
        | SETARparams(coeffs1,coeffs2,threshold,delay) -> let ar1 = Array.zeroCreate coeffs1.Length |> Array.mapi (fun i _ -> Leaf(Parameter,None,i))
                                                                                                    |> Array.mapi (fun i p -> Leaf(Variable,None,i) |> ( *. ) p)
                                                                                                    |> Array.reduce ( +. )
                                                                                                    |> ( +. ) (Leaf(Innovation,None,0))

                                                          let ar2 = Array.zeroCreate coeffs2.Length |> Array.mapi (fun i _ -> Leaf(Parameter,None,i+coeffs1.Length))
                                                                                                    |> Array.mapi (fun i p -> Leaf(Variable,None,i+coeffs1.Length) |> ( *. ) p)
                                                                                                    |> Array.reduce ( +. )
                                                                                                    |> ( +. ) (Leaf(Innovation,None,0))
                                                          
                                                          let cond = (Leaf(Parameter,None,coeffs1.Length+coeffs2.Length) <. Leaf(Variable,None,coeffs1.Length+coeffs2.Length))
                                                          (cond *. ar1) +. ((Leaf(Constant,Some 1.0,0) -. cond) *. ar2) 

    let buildDefaultGraph (ModelType(name,parameters)) = 
        match parameters with
        | MAparams(coeffs) | ARparams(coeffs) -> let state = GraphState(coeffs, Array.zeroCreate coeffs.Length, [|0.0|], [|0.0|])
                                                 Graph( state, buildSkeleton (ModelType(name,parameters)) )
        | SETARparams(coeffs1,coeffs2,threshold,delay) -> let state = GraphState(Array.concat [|coeffs1;coeffs2;[|threshold|];[|float(delay)|]|], Array.zeroCreate (coeffs1.Length+coeffs2.Length+1),[|0.0|], [|0.0|])
                                                          Graph( state, buildSkeleton (ModelType(name,parameters)))

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

    let buildUpdatingStrategy (g:Graph) (ModelType(_, parameters)) = 
        match parameters with
        | MAparams(coeffs) -> coeffs.Length |> Array.zeroCreate |> Array.mapi (fun i _ -> updateStrategyMA coeffs.Length i) |> UpdateVariableStrategy
        | ARparams(coeffs) -> coeffs.Length |> Array.zeroCreate |> Array.mapi (fun i _ -> updateStrategyAR coeffs.Length i) |> UpdateVariableStrategy
        | SETARparams(coeffs1,coeffs2,_,delay) -> coeffs1.Length+coeffs2.Length+1 |> Array.zeroCreate |> Array.mapi (fun i _ -> updateStrategySETAR coeffs1.Length coeffs2.Length delay i) |> UpdateVariableStrategy

    let build name parameters =
        let modeltype = ModelType(name, parameters) // check if the parameters correspond to the model name.
        let graph = buildDefaultGraph modeltype
        let updateStrategy = buildUpdatingStrategy graph modeltype
        T(name, graph, updateStrategy)

    let updateVariables (UpdateVariableStrategy(pullfrom)) (GraphState(p,v,innov,prevResult)) = 
        Array.zeroCreate v.Length |> Array.mapi (fun i _ -> match pullfrom.[i] with  
                                                            | Some (Innovation, idx) -> innov.[idx]
                                                            | Some (Parameter, idx) -> p.[idx]
                                                            | Some (Variable, idx) -> v.[idx]
                                                            | Some (PreviousResult, idx) -> prevResult.[idx]  // Make sure the idx is a possible index of array.
                                                            | Some (Constant,_) -> invalidArg "constant" "Cannot update a Variable from a Constant"
                                                            | None -> v.[i])

    let foldGraphTimeSeries nextParamF nextVarF nextGraphF nextInnovF (array:float array) graph = 
        let (Graph(state,sk)) = graph
        let updateGraphState state x = 
            let currentResult = Graph.forwardPass (Graph(state,sk))
            GraphState(nextParamF x state, state |> nextGraphF x currentResult |>  nextVarF x, nextInnovF x state, [|currentResult|])
        Array.scan updateGraphState state array |> Array.skip 1

    let sample (n:int) (T(_,graph,updateStrat)) = 
        let result = Array.zeroCreate n
        let distr = Distributions.Norm(0.0, 1.0) |> create
        foldGraphTimeSeries (fun _ (GraphState(p,_,_,_)) -> p)
                            (fun _ state -> updateVariables updateStrat state)
                            (fun _ currentResult (GraphState(p,v,i,_)) -> GraphState(p,v,i,[|currentResult|]))
                            (fun _ (GraphState(_,_,innov,_)) -> [| for i in 0..innov.Length-1 do distr |> Distributions.sample |])
                            result
                            graph
                |> Array.map (fun (GraphState(_,_,_,prev)) -> prev.[0])

    let fit (array:float array) (T(name,graph,updateStrat)) = 
        let len = array.Length
        let (Graph(GraphState(initParam,initVariables,initInnov,initPrev),initSkeleton)) = graph

        let updateGraphWithTruth (dataPoint:float) (expectation:float) (GraphState(p,v,i,prev)) = 
            match name with
            | MA -> GraphState(p,v,[|dataPoint - expectation|],prev)
            | AR -> GraphState(p,v,i,[|dataPoint|])
            | SETAR -> GraphState(p,v,i,[|dataPoint|])

        let mutable finalVariables = initVariables

        let leastSquareFunction pa = 
            let tmp = foldGraphTimeSeries (fun _ (GraphState(p,_,_,_)) -> p)
                                          (fun _ g -> updateVariables updateStrat g)
                                          (fun dataPoint expect g -> updateGraphWithTruth dataPoint expect g)
                                          (fun _ (GraphState(_,_,innov,_)) -> Array.zeroCreate innov.Length)
                                          array
                                          (Graph(GraphState(pa,initVariables,initInnov,initPrev),initSkeleton))
            let (GraphState(_,v,_,_)) = Array.last tmp
            finalVariables <- v
            tmp |> Array.map2 (fun x (GraphState(_,_,_,prev)) -> (x - prev.[0]) * (x - prev.[0])) array
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
        T(name, Graph(GraphState(fittedParameters,finalVariables,initInnov,initPrev),initSkeleton),updateStrat)

    let conditionalExpectation steps (T(name,graph,updateStrat)) = 
        foldGraphTimeSeries (fun _ (GraphState(p,_,_,_)) -> p)
                            (fun _ g -> updateVariables updateStrat g)
                            (fun _ currentResult (GraphState(p,v,i,_)) -> GraphState(p,v,i,[|currentResult|]))
                            (fun _ (GraphState(_,_,innov,_)) -> Array.zeroCreate innov.Length)
                            (Array.zeroCreate steps)
                            graph
                |> Array.map (fun (GraphState(_,_,_,prev)) -> prev.[0])


                       

       
      
    







    



    

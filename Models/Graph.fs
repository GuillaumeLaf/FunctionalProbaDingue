namespace Models

module Graph = 
    let inline ( +. ) (N1:Skeleton) (N2:Skeleton) = Node(Addition, N1, N2)
    let inline ( *. ) (N1:Skeleton) (N2:Skeleton) = Node(Multiplication, N1, N2)
    let inline ( -. ) (N1:Skeleton) (N2:Skeleton) = Node(Substraction, N1, N2)
    let inline ( <. ) (N1:Skeleton) (N2:Skeleton) = Node(LessThan, N1, N2)

    let emptyState = GraphState([||],[||],[||],[||],[||])

    module Skeleton = 
        let fold nodeF leafV sk = 
            let rec loop n k = 
                match n with
                    | Node(op,left,right) -> nodeF op (loop left) (loop right) n k
                    | Leaf(input,x,idx,pullFrom) -> leafV input x idx pullFrom n k
            loop sk id

        let height skeleton = 
            fold (fun _ kl kr _ k -> kl (fun lacc -> kr (fun racc -> (1 + max lacc racc) |> k)))
                 (fun _ _ _ _ _ k -> 1 |> k)
                 skeleton
 
        let countNodeByInput skeleton = 
            fold (fun op kl kr n k -> kl (fun lacc -> kr (fun racc -> (fun l -> lacc (racc l)) |> k)))
                 (fun input x idx _ _ k -> (fun l -> input::l) |> k)
                 skeleton
                 []
                |> Array.ofList
                |> Array.countBy id

        let updatingStrategy skeleton = 
            fold (fun op kl kr n k -> kl (fun lacc -> kr (fun racc -> (fun l -> lacc (racc l)) |> k)))
                 (fun input x idx pullFrom _ k -> (fun l -> (input,(idx, pullFrom))::l) |> k)
                 skeleton
                 []
                |> Array.ofList 
                |> Array.filter (fun x -> (fst x) = Variable)
                |> Array.sortBy (fun x -> fst (snd x))
                |> Array.map (fun x -> snd (snd x))

        let valueOfConstants skeleton = 
            fold (fun op kl kr n k -> kl (fun lacc -> kr (fun racc -> (fun acc -> lacc (racc acc)) |> k)))
                 (fun input x idx pullFrom _ k -> (fun acc -> if input = Constant then (idx,x)::acc else acc) |> k)
                 skeleton
                 []
            |> Array.ofList
            |> Array.sortBy (fun x -> fst x)
            |> Array.map (fun x -> (snd x) |> Option.defaultValue 0.0)

        // 'groups' array contains the group of each Innovation Node (where the idx of array is the initial idx of the Node).
        let groupInnovations (groups:int []) skeleton = 
            fold (fun op kl kr n k -> kl (fun lacc -> kr (fun racc -> Node(op,lacc,racc) |> k)))
                 (fun input x idx pullFrom n k -> (if input = Innovation then (Leaf(Innovation,x,groups.[idx],pullFrom)) else (Leaf(input,x,idx,pullFrom))) |> k)
                 skeleton

    let forwardPass (Graph(GraphState(p,v,innov,prevResult,constant),skeleton)) = 
        Skeleton.fold (fun op kl kr _ k -> match op with
                                            | Addition -> kl (fun lacc -> kr (fun racc -> (lacc + racc) |> k))
                                            | Multiplication -> kl (fun lacc -> kr (fun racc ->  (lacc * racc) |> k))
                                            | Substraction -> kl (fun lacc -> kr (fun racc -> (lacc - racc) |> k))
                                            | LessThan -> kl (fun lacc -> kr (fun racc -> (if lacc < racc then 1.0 else 0.0) |> k))
                                            )
                      (fun input x idx _ _ k -> match input with
                                                | Innovation -> innov.[idx] |> k   // input arrays must at least be the same length as the number of innovation nodes.
                                                | Parameter -> p.[idx] |> k
                                                | Variable -> v.[idx] |> k
                                                | PreviousResult -> prevResult.[idx] |> k // never reached
                                                | Constant -> constant.[idx] |> k)
                      skeleton

    let getUpdatingStrategy (Graph(state,skeleton)) = Skeleton.updatingStrategy skeleton

    let updateVariables (UpdateVariableStrategy(pullfrom)) (GraphState(p,v,innov,prevResult,constant)) = 
        Array.zeroCreate v.Length |> Array.mapi (fun i _ -> match pullfrom.[i] with  
                                                            | Some (Innovation, idx) -> innov.[idx]
                                                            | Some (Parameter, idx) -> p.[idx]
                                                            | Some (Variable, idx) -> v.[idx]
                                                            | Some (PreviousResult, idx) -> prevResult.[idx]  // Make sure the idx is a possible index of array.
                                                            | Some (Constant,idx) -> constant.[idx] 
                                                            | None -> v.[i])   

    module TimeSerie = 
        let fold currentResultF nextParamF nextVarF nextGraphF nextInnovF (array:float array) (Graph(initialState,sk)) = 
            let (GraphState(_,_,_,_,c)) = initialState
            let updateGraphState state x = 
                let currentResult = currentResultF (Graph(state,sk))
                GraphState(nextParamF x state, state |> nextGraphF x currentResult |>  nextVarF x, nextInnovF x state, [|currentResult|],c)
            Array.scan updateGraphState initialState array |> Array.skip 1

        let updateGraphWithTruth name (newDataPoint:float) (expectation:float) (GraphState(p,v,i,prev,c)) = 
            match name with
            | MA -> GraphState(p,v,[|newDataPoint - expectation|],prev,c)
            | AR -> GraphState(p,v,i,[|newDataPoint|],c)
            | SETAR -> GraphState(p,v,i,[|newDataPoint|],c)

        let rollingGraphStates (array:float array) (T(name,graph,updateStrat)) = 
            let updateGraph = updateGraphWithTruth name
            fold forwardPass
                 (fun _ (GraphState(p,_,_,_,_)) -> p)
                 (fun _ state -> updateVariables updateStrat state)
                 (fun dataPoint expect state -> updateGraph dataPoint expect state)
                 (fun _ (GraphState(_,_,innov,_,_)) -> Array.zeroCreate innov.Length)
                 array
                 graph

        let realizations stateTS = stateTS |> Array.map (fun (GraphState(_,_,_,prev,_)) -> prev.[0])
            


                    
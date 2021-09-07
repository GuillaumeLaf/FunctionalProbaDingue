namespace Models

module Graph = 
    let inline ( +. ) (N1:Skeleton) (N2:Skeleton) = Node(Addition, N1, N2)
    let inline ( *. ) (N1:Skeleton) (N2:Skeleton) = Node(Multiplication, N1, N2)
    let inline ( -. ) (N1:Skeleton) (N2:Skeleton) = Node(Substraction, N1, N2)
    let inline ( <. ) (N1:Skeleton) (N2:Skeleton) = Node(LessThan, N1, N2)

    let (<*>) = Monad.apply
    let (<!>) = Monad.map

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

    let updateVariables (UpdateVariableStrategy(pullfrom)) (GraphState(p,v,innov,prevResult,constant)) = 
        Array.zeroCreate v.Length |> Array.mapi (fun i _ -> match pullfrom.[i] with  
                                                            | Some (Innovation, idx) -> innov.[idx]
                                                            | Some (Parameter, idx) -> p.[idx]
                                                            | Some (Variable, idx) -> v.[idx]
                                                            | Some (PreviousResult, idx) -> prevResult.[idx]  // Make sure the idx is a possible index of array.
                                                            | Some (Constant,idx) -> constant.[idx] 
                                                            | None -> v.[i])

    let getUpdatingStrategy (Graph(state,skeleton)) = Skeleton.updatingStrategy skeleton 

    module TimeSerie = 
        let updateStateDataM f = 
            let innerFunc (state:GraphState) = f state  
            Monad.M innerFunc

        let forwardPassM skeleton = 
            let innerFunc (GraphState(p,v,i,prev,c)) = 
                let result = forwardPass (Graph(GraphState(p,v,i,prev,c),skeleton))
                result, (GraphState(p,v,i,[|result|],c))
            Monad.M innerFunc            
        
        let reorganizeVariablesM updateStrat = updateStateDataM (fun (GraphState(p,v,i,prev,c)) -> let newVariables = updateVariables updateStrat (GraphState(p,v,i,prev,c))
                                                                                                   newVariables, GraphState(p,newVariables,i,prev,c) )
        let randomInnovationsM distr = updateStateDataM (fun (GraphState(p,v,innov,prev,c)) -> let newInnovations = [| for i in 0..innov.Length-1 do distr |> Distributions.sample |]
                                                                                               newInnovations,GraphState(p,v,newInnovations,prev,c) ) 
        let inactiveInnovationsM = updateStateDataM (fun (GraphState(p,v,innov,prev,c)) -> let newInnovations = Array.zeroCreate innov.Length
                                                                                           newInnovations,GraphState(p,v,newInnovations,prev,c) ) 

        let reorganizeVariableWithTruthM name updateStrat truthPoint = 
            updateStateDataM (fun (GraphState(p,v,i,prev,c)) -> let updatedGraph = match name with
                                                                                    | MA -> GraphState(p,v,[|truthPoint - prev.[0]|],prev,c) 
                                                                                    | AR ->  GraphState(p,v,i,[|truthPoint|],c)
                                                                                    | SETAR -> GraphState(p,v,i,[|truthPoint|],c) 
                                                                let newVariables = updateVariables updateStrat updatedGraph 
                                                                newVariables, GraphState(p,newVariables,i,prev,c)
                                                                ) 

        let conditionalExpectationM updateStrat sk = 
            let innerFunc fwdPass variables innovations = fwdPass
            innerFunc <!> forwardPassM sk
                      <*> reorganizeVariablesM updateStrat
                      <*> inactiveInnovationsM

        let oneStepRollingForecastM name updateStrat sk truthPoint =
            let innerFunc fwdPass variables innovations = fwdPass
            innerFunc <!> forwardPassM sk
                      <*> reorganizeVariableWithTruthM name updateStrat truthPoint
                      <*> inactiveInnovationsM

(*        let rollingConditionalExpectationM steps updateStrat sk truthPoints =
            truthPoints |> List.map (fun _ -> conditionalExpectationM updateStrat sk)
                        |> Monad.sequence
                        |> Monad.map (List.head)*)

        let fold monad initState array = 
            array |> Array.map (fun _ -> monad)
                  |> Array.mapFold (fun state mo -> Monad.run mo state) initState

        let fold1 monad initState array = 
            array |> Array.map (fun x -> monad x)
                  |> Array.mapFold (fun state mo -> Monad.run mo state) initState

        
            


                    
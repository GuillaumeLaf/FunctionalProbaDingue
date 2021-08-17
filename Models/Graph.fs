namespace Models

module Graph = 
    let inline ( +. ) (N1:Skeleton) (N2:Skeleton) = Node(Addition, N1, N2)
    let inline ( *. ) (N1:Skeleton) (N2:Skeleton) = Node(Multiplication, N1, N2)
    let inline ( -. ) (N1:Skeleton) (N2:Skeleton) = Node(Substraction, N1, N2)
    let inline ( <. ) (N1:Skeleton) (N2:Skeleton) = Node(LessThan, N1, N2)

    let foldSkeleton nodeF leafV sk = 
        let rec loop n k = 
            match n with
                | Node(op,left,right) -> nodeF op (loop left) (loop right) n k
                | Leaf(input,x,idx,pullFrom) -> leafV input x idx pullFrom n k
        loop sk id

    let heightSkeleton skeleton = 
        foldSkeleton (fun _ kl kr _ k -> kl (fun lacc -> kr (fun racc -> (1 + max lacc racc) |> k)))
                     (fun _ _ _ _ _ k -> 1 |> k)
                     skeleton

    let forwardPass (Graph(GraphState(p,v,innov,prevResult,constant),skeleton)) = 
        foldSkeleton (fun op kl kr _ k -> match op with
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
                                               | Constant -> x |> Option.defaultValue 0.0 |> k)
                     skeleton

    let countNodeByInput (Graph(_,skeleton)) = 
        foldSkeleton (fun op kl kr n k -> kl (fun lacc -> kr (fun racc -> (fun l -> lacc (racc l)) |> k)))
                     (fun input x idx _ _ k -> (fun l -> input::l) |> k)
                     skeleton
                     []
            |> Array.ofList
            |> Array.countBy id

    let countNodeByInputFromSK sk = 
        let graph = Graph(GraphState([|0.0|],[|0.0|],[|0.0|],[|0.0|],[|0.0|]),sk)
        countNodeByInput graph

    let getUpdatingStrategy (Graph(_,skeleton)) = 
        foldSkeleton (fun op kl kr n k -> kl (fun lacc -> kr (fun racc -> (fun l -> lacc (racc l)) |> k)))
                     (fun input x idx pullFrom _ k -> (fun l -> (input,(idx, pullFrom))::l) |> k)
                     skeleton
                     []
            |> Array.ofList 
            |> Array.filter (fun x -> (fst x) = Variable)
            |> Array.sortBy (fun x -> fst (snd x))
            |> Array.map (fun x -> snd (snd x))

    let getValueOfConstants skeleton = 
        foldSkeleton (fun op kl kr n k -> kl (fun lacc -> kr (fun racc -> (fun acc -> lacc (racc acc)) |> k)))
                     (fun input x idx pullFrom _ k -> (fun acc -> if input = Constant then (idx,x)::acc else acc) |> k)
                     skeleton
                     []
        |> Array.ofList
        |> Array.sortBy (fun x -> fst x)
        |> Array.map (fun x -> (snd x) |> Option.defaultValue 0.0)


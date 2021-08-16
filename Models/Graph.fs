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
                | Leaf(input,x,idx) -> leafV input x idx n k
        loop sk id

    let heightSkeleton skeleton = 
        foldSkeleton (fun _ kl kr _ k -> kl (fun lacc -> kr (fun racc -> (1 + max lacc racc) |> k)))
                     (fun _ _ _ _ k -> 1 |> k)
                     skeleton

    let forwardPass (Graph(GraphState(p,v,innov,prevResult),skeleton)) = 
        foldSkeleton (fun op kl kr _ k -> match op with
                                            | Addition -> kl (fun lacc -> kr (fun racc -> (lacc + racc) |> k))
                                            | Multiplication -> kl (fun lacc -> kr (fun racc ->  (lacc * racc) |> k))
                                            | Substraction -> kl (fun lacc -> kr (fun racc -> (lacc - racc) |> k))
                                            | LessThan -> kl (fun lacc -> kr (fun racc -> (if lacc < racc then 1.0 else 0.0) |> k))
                                            )
                     (fun input x idx _ k -> match input with
                                             | Innovation -> innov.[idx] |> k   // input arrays must at least be the same length as the number of innovation nodes.
                                             | Parameter -> p.[idx] |> k
                                             | Variable -> v.[idx] |> k
                                             | PreviousResult -> prevResult.[idx] |> k // never reached
                                             | Constant -> x |> Option.defaultValue 0.0 |> k)
                     skeleton

    let countNodeByGroup skeleton = 
        foldSkeleton (fun op kl kr n k -> kl (fun lacc -> kr (fun racc -> (fun (l:Input list) -> lacc (racc l)) |> k)))
                     (fun input x idx _ k -> (fun (l:Input list) -> input::l) |> k)
                     skeleton
                     []
            |> Array.ofList
            |> Array.countBy id


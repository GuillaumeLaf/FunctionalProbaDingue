namespace Models

module SkeletonTree = 
    let inline ( .+. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Addition, N1, N2)
    let inline ( .*. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Multiplication, N1, N2)

    let equalInputs input1 input2 = 
        match (input1,input2) with
        | Parameter(idx1),Parameter(idx2) -> idx1 = idx2 
        | Variable(idx1),Variable(idx2) -> idx1 = idx2 
        | Innovation(idx1),Innovation(idx2) -> idx1 = idx2 
        | Constant(value1),Constant(value2) -> value1 = value2
        | _ -> false

    let inline fold node1F node2F leafV sk = 
        let rec loop n k = 
            match n with
                | Node1(op,next) -> node1F op (loop next) n k
                | Node2(op,left,right) -> node2F op (loop left) (loop right) n k
                | Leaf(input) -> leafV input n k
        loop sk id

    let shift parameterShift variableShift innovationShift skeleton = 
        fold (fun op nk _ k -> nk (fun nacc -> Node1(op, nacc) |> k))
             (fun op kl kr _ k -> kl (fun lacc -> kr (fun racc -> Node2(op, lacc, racc) |> k)))
             (fun input _ k -> match input with
                                | Parameter(idx) -> Leaf(Parameter(idx+parameterShift)) |> k
                                | Variable(idx) -> Leaf(Variable(idx+variableShift)) |> k
                                | Innovation(idx) -> Leaf(Innovation(idx+innovationShift)) |> k
                                | Constant(value) -> Leaf(Constant(value)) |> k)
             skeleton

    // count the number of leaves by type: 
    // index 0 = Parameters; index 1 = Variables; index 2 = Innovations
    let countLeaves skeleton = 
        let count = Array.zeroCreate 3
        fold (fun op nk _ k -> nk (fun nacc -> () |> k))
             (fun op kl kr _ k -> kl (fun lacc -> kr (fun racc -> () |> k)))
             (fun input _ k -> match input with
                                 | Parameter(_) -> (count.[0] <- count.[0] + 1) |> k
                                 | Variable(_) -> (count.[1] <- count.[1] + 1) |> k
                                 | Innovation(_) -> (count.[2] <- count.[2] + 1) |> k
                                 | Constant(_) -> () |> k
                                 )
             skeleton |> ignore
        count

    let height skeleton = 
        fold (fun op nk _ k -> nk (fun nacc -> (nacc + 1) |> k))
             (fun _ kl kr _ k -> kl (fun lacc -> kr (fun racc -> (1 + max lacc racc) |> k)))
             (fun _ _ k -> 1 |> k)
             skeleton

    let deactivateInnovations skeleton = 
        fold (fun op nk n k -> nk (fun nacc -> Node1(op,nacc) |> k))
             (fun op kl kr n k -> kl (fun lacc -> kr (fun racc -> Node2(op,lacc,racc) |> k)))
             (fun input n k -> match input with
                                | Innovation(_) -> Leaf(Constant(0.0)) |> k
                                | _ -> Leaf(input) |> k)
             skeleton

    let gradientInputForParameter idxParam = function
        | Parameter(idx) -> if idx = idxParam then Leaf(Constant(1.0)) else Leaf(Constant(0.0))
        | _ -> Leaf(Constant(0.0))

    // The first element of the tuple is the gradient skeleton
    let gradientSkeletonForParameter idxParam skeleton = 
        fold (fun op nk _ k -> match op with
                                | Apply(f) -> nk (fun nacc -> (Node1(Apply(f), fst nacc),Node1(Apply(f), snd nacc)) |> k))
             (fun op kl kr _ k -> match op with
                                    | Addition -> kl (fun lacc -> kr (fun racc -> (Node2(Addition,fst lacc,fst racc),Node2(Addition,snd lacc,snd racc)) |> k)) 
                                    | Multiplication -> kl (fun lacc -> kr (fun racc -> let lg, l = lacc  
                                                                                        let rg, r = racc
                                                                                        ((Node2(Addition,Node2(Multiplication,lg,r),Node2(Multiplication,rg,l))),Node2(Multiplication,snd lacc,snd racc)) |> k))
                                    | Substraction -> kl (fun lacc -> kr (fun racc ->  (Node2(Substraction,fst lacc,fst racc),Node2(Substraction,snd lacc,snd racc)) |> k)))
             (fun input _ k -> match input with
                                | Parameter(idx) as x -> (gradientInputForParameter idxParam x,Leaf(Parameter(idx))) |> k
                                | Variable(idx) as x -> (Leaf(Constant(0.0)),Leaf(Variable(idx))) |> k
                                | Innovation(idx) as x -> (Leaf(Constant(0.0)),Leaf(Innovation(idx))) |> k  
                                | Constant(value) as x -> (Leaf(Constant(0.0)),Leaf(Constant(value))) |> k)
             skeleton 


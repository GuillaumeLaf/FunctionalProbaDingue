namespace Models

module SkeletonTree = 
    let inline ( .+. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Addition, N1, N2)
    let inline ( .*. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Multiplication, N1, N2)

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
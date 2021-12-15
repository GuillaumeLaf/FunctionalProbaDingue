namespace Models

module SkeletonTree = 
    let inline ( .+. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Addition, N1, N2)
    let inline ( .*. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Multiplication, N1, N2)
    let inline ( .-. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Substraction, N1, N2)
    let inline ( ./. ) (N1:Skeleton<'T>) (N2:Skeleton<'T>) = Node2(Division, N1, N2)

    let simplifyAddition lsk rsk =
        match (lsk,rsk) with
        | Leaf(Constant(0.0)), _ -> rsk
        | _, Leaf(Constant(0.0)) -> lsk
        | _,_ -> Node2(Addition,lsk,rsk)

    let simplifySubstraction lsk rsk = 
        match (lsk,rsk) with
        | Leaf(Constant(0.0)), _ -> rsk
        | _, Leaf(Constant(0.0)) -> lsk
        | _,_ -> Node2(Substraction,lsk,rsk)

    let simplifyMultiplication lsk rsk = 
        match (lsk,rsk) with
        | Leaf(Constant(0.0)), _ -> Leaf(Constant(0.0))
        | _, Leaf(Constant(0.0)) -> Leaf(Constant(0.0))
        | Leaf(Constant(1.0)), _ -> rsk
        | _, Leaf(Constant(1.0)) -> lsk
        | _, _ -> Node2(Multiplication,lsk,rsk)

    let simplifyDivision lsk rsk = 
        match (lsk,rsk) with
        | Leaf(Constant(0.0)), _ -> Leaf(Constant(0.0))
        | _, Leaf(Constant(0.0)) -> invalidArg "rsk" "Cannot divide by zero in division node."
        | _, Leaf(Constant(1.0)) -> lsk
        | _, _ -> Node2(Division,lsk,rsk)

    let simplifyPolynomial sk = function
        | Polynomial(1.0) -> sk
        | x -> Node1(x,sk)

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
    // Careful ! Doesn't count the unique nodes ! (only the row number of nodes present in skeleton)
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

    // count the number of leaves by type: 
    // index 0 = Parameters; index 1 = Variables; index 2 = Innovations
    let countUniqueLeaves skeleton = 
        let count = [|[];[];[]|]
        fold (fun op nk _ k -> nk (fun nacc -> () |> k))
             (fun op kl kr _ k -> kl (fun lacc -> kr (fun racc -> () |> k)))
             (fun input _ k -> match input with
                                 | Parameter(idx) -> (count.[0] <- idx :: count.[0]) |> k
                                 | Variable(idx) -> (count.[1] <- idx :: count.[1]) |> k
                                 | Innovation(idx) -> (count.[2] <- idx :: count.[2]) |> k
                                 | Constant(_) -> () |> k
                                 )
             skeleton |> ignore
        count |> Array.map (fun x -> x |> List.distinct |> List.length)

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

    let _gradientInputForParameter idxParam = function
        | Parameter(idx) -> if idx = idxParam then Leaf(Constant(1.0)) else Leaf(Constant(0.0))
        | _ -> Leaf(Constant(0.0))

    // The first element of the tuple is the gradient skeleton
    let gradientSkeletonForParameter idxParam skeleton = 
        fold (fun op nk _ k -> match op with
                                | Exponential as x -> nk (fun nacc -> (fst nacc .*. Node1(x, snd nacc),Node1(x, snd nacc)) |> k)
                                | Polynomial(e) as x -> nk (fun nacc -> (fst nacc .*. Leaf(Constant(e)) .*. Node1(Polynomial(e-1.0), snd nacc),Node1(x, snd nacc)) |> k)
                                )
             (fun op kl kr _ k -> match op with
                                    | Addition -> kl (fun lacc -> kr (fun racc -> (fst lacc .+. fst racc, snd lacc .+. snd racc) |> k)) 
                                    | Multiplication -> kl (fun lacc -> kr (fun racc -> let lg, l = lacc  
                                                                                        let rg, r = racc
                                                                                        ((lg .*. r) .+. (rg .*. l), snd lacc .*. snd racc) |> k))
                                    | Substraction -> kl (fun lacc -> kr (fun racc ->  (fst lacc .-. fst racc, snd lacc .-. snd racc) |> k))
                                    | Division -> kl (fun lacc -> kr (fun racc -> let lg, l = lacc  
                                                                                  let rg, r = racc
                                                                                  (((lg .*. r) .-. (l .*. rg)) ./. (Node1(Polynomial(2.0),r)),snd lacc ./. snd racc) |> k))
                                    )
             (fun input _ k -> match input with
                                | Parameter(idx) as x -> (_gradientInputForParameter idxParam x,Leaf(Parameter(idx))) |> k
                                | Variable(idx) as x -> (Leaf(Constant(0.0)),Leaf(Variable(idx))) |> k
                                | Innovation(idx) as x -> (Leaf(Constant(0.0)),Leaf(Innovation(idx))) |> k  
                                | Constant(value) as x -> (Leaf(Constant(0.0)),Leaf(Constant(value))) |> k)
             skeleton

    let simplify skeleton = 
        fold (fun op nk _ k -> match op with
                                | Polynomial(n) as x -> nk (fun nacc -> simplifyPolynomial nacc x |> k)
                                | _ -> nk (fun nacc -> Node1(op, nacc) |> k)
                                )
             (fun op kl kr _ k -> match op with
                                    | Addition -> kl (fun lacc -> kr (fun racc -> simplifyAddition lacc racc |> k)) 
                                    | Multiplication -> kl (fun lacc -> kr (fun racc -> simplifyMultiplication lacc racc |> k))
                                    | Substraction -> kl (fun lacc -> kr (fun racc ->  simplifySubstraction lacc racc |> k))
                                    | Division -> kl (fun lacc -> kr (fun racc ->  simplifyDivision lacc racc |> k))
                                    )
             (fun input _ k -> match input with
                                | Parameter(idx) as x -> Leaf(Parameter(idx)) |> k
                                | Variable(idx) as x -> Leaf(Variable(idx)) |> k
                                | Innovation(idx) as x -> Leaf(Innovation(idx)) |> k  
                                | Constant(value) as x -> Leaf(Constant(value)) |> k)
             skeleton

    let gradientSkeleton skeleton = 
        // problem with counting number of nodes ! (nodes could have same index and thus be 'unique')
        let nParameters = Array.get (countUniqueLeaves skeleton) 0 
        Array.zeroCreate nParameters |> Array.mapi (fun idx _ -> gradientSkeletonForParameter idx skeleton |> fst |> simplify)




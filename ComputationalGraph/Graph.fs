namespace ComputationalGraph

open System.Runtime.CompilerServices
open FSharpPlus.Data
open FSharpPlus.Control
open FSharpPlus
open GraphType


// The most important part of this project.
// Creating a computational graph eases creation of complex computations. 
// The ideal would be to begin with a classical computational graph and then convert/compile it to blazingly fast arrays operations. 
[<RequireQualifiedAccess>]
module Graph =   
    
    let inline add g1 g2 = Addition(g1,g2)
    let inline multiply g1 g2 = Multiplication(g1,g2)

    // Check equality between two graphs.
    // Graphs are considered equal if all their nodes are equal and in the same order
    // [TESTED]
    let equal g1 g2 = 
        let opCont l1 r1 l2 r2 f : Cont<'a,bool> = monad { let! xl = f l1 l2
                                                           let! xr = f r1 r2
                                                           return xl && xr }
        let rec loop g1 g2 = monad {
            match g1,g2 with
            | Input(i1),Input(i2) -> return i1 = i2
            | Polynomial(g1,e1), Polynomial(g2,e2) -> let! x = loop g1 g2
                                                      return e1 = e2 && x
            | Addition(l1,r1),Addition(l2,r2) -> return! opCont l1 r1 l2 r2 loop
            | Substraction(l1,r1),Substraction(l2,r2) -> return! opCont l1 r1 l2 r2 loop
            | Multiplication(l1,r1),Multiplication(l2,r2) -> return! opCont l1 r1 l2 r2 loop
            | _ -> return false         // !!!
        }
        Cont.run (loop g1 g2) id
    
    // Catamorphism for folding over a graph tree. 
    // Each function takes the current node as first input. 
    let inline cataFoldX constF polyF addF subF multF inputF (x:Graph<'T>) = 
        let inline opCont opF x lg rg f : Cont<'a,'b> = monad { let! xl = f lg
                                                                let! xr = f rg
                                                                return opF x xl xr }
        let rec loop g = monad {
            match g with
            | Constant(value) as x -> return constF x value
            | Polynomial(g,e) as x -> let! r = loop g
                                      return polyF x r e
            | Addition(lg,rg) as x -> return! opCont addF x lg rg loop
            | Substraction(lg,rg) as x -> return! opCont subF x lg rg loop
            | Multiplication(lg,rg) as x -> return! opCont multF x lg rg loop
            | Input(i) as x -> return inputF x i
        }
        Cont.run (loop x) id

    // Catamorphism for folding over a graph tree. 
    // Functions DON'T take the current node as input. 
    let inline cataFold constF polyF addF subF multF inputF = 
        cataFoldX (fun _ -> constF) (fun _ -> polyF) (fun _ -> addF) (fun _ -> subF) (fun _ -> multF) (fun _ -> inputF)

    // Collect in an array all the subgraphs from the graph 'x'
    // [TESTED]
    let collectSubGraphs (x:Graph<'T>) = 
        cataFoldX (fun x _ acc -> x::acc)
                  (fun x g _ acc -> x::(g acc))
                  (fun x lacc racc acc -> x::(racc >> lacc) acc)
                  (fun x lacc racc acc -> x::(racc >> lacc) acc)
                  (fun x lacc racc acc -> x::(racc >> lacc) acc)
                  (fun x _ acc -> x::acc)
                  x [] |> Array.ofList

    // Concatenate in an array all the 'Input' nodes of the graph 'x'. 
    let collectInputs (x:Graph<'T>) = 
        cataFold (fun _ acc -> acc)
                 (fun g _ acc -> g acc)
                 (fun lacc racc acc -> lacc (racc acc))
                 (fun lacc racc acc -> lacc (racc acc))
                 (fun lacc racc acc -> lacc (racc acc))
                 (fun i acc -> i::acc)
                 x [] |> Array.ofList

    let collectUniqueInputs (g:Graph<'T>) = collectInputs >> Array.distinct <| g
    let collectUniqueParameters (g:Graph<'T>) = collectUniqueInputs >> Array.filter (function | Parameter(_,_) -> true | _ -> false) <| g

    // From an 'Array' of 'BasicInput's count the number of 'BasicInput's for each group.
    // For instance, the output array is of the form : [|Parameter(0,2);Variable(1,3)|], 
    // It translates into a graph with 2 'Parameter's in group 0,
    // and 3 'Variable's in group 1.
    // DOESN'T count unique 'BasicInput's.
    let private countInputByGroup = 
        Array.countBy (function | Parameter(grpidx,_) -> Parameter(grpidx,0)
                                | Variable(grpidx,_) -> Variable(grpidx,0)
                                | Innovation(grpidx,_) -> Innovation(grpidx,0))
        >> Array.map (function | Parameter(grpidx,_),c -> Parameter(grpidx,c)
                                | Variable(grpidx,_),c -> Variable(grpidx,c)
                                | Innovation(grpidx,_),c -> Innovation(grpidx,c))
        >> Array.sortBy (function | Parameter(grpidx,_) | Variable(grpidx,_) | Innovation(grpidx,_) -> grpidx)

    // Count the 'BasicInput's contained in the 'Graph' as an array.
    // [TESTED]
    let count (g:Graph<'T>) = collectInputs >> countInputByGroup <| g
        
    // Count the UNIQUE 'BasicInput's contained in the 'Graph' as an array.
    // [TESTED]
    let countUnique (g:Graph<'T>) = collectInputs >> Array.countBy id >> Array.map fst >> countInputByGroup <| g

    // Get an array with each unique group indices
    // [TESTED]
    let countGroups (g:Graph<'T>) = count >> Array.collect (function | Parameter(grp,_) -> [|grp|]
                                                                     | Variable(grp,_) -> [|grp|]
                                                                     | Innovation(grp,_) -> [|grp|]) >> Array.sort <| g
        
    // Change the 'Group Index' of some 'BasicInput' from 'oldGrp' to 'newGrp'.
    // [TESTED]
    let changeGroup (oldGrp:int) (newGrp:int) (g:Graph<'T>) =
        cataFold (fun v -> Constant(v))
                 (fun g e -> Polynomial(g,e))
                 (fun l r -> Addition(l,r))
                 (fun l r -> Substraction(l,r))
                 (fun l r -> Multiplication(l,r))
                 (function | Parameter(grpidx,idx) as x -> if grpidx=oldGrp then Input(Parameter(newGrp,idx)) else Input(x)
                           | Variable(grpidx,idx) as x -> if grpidx=oldGrp then Input(Variable(newGrp,idx)) else Input(x)
                           | Innovation(grpidx,idx) as x -> if grpidx=oldGrp then Input(Innovation(newGrp,idx)) else Input(x))
                 g

    // Shift the individual index of a given 'BasicInput' ('t') by 'i'.
    // All groups are shifted.
    // e.g. Parameter(_,1) - > Parameter(_, 3) if 'i' is 2 and 't' is 'Parameter'
    // [TESTED]
    let shift (t:int*int->BasicInput) i (g:Graph<'T>) = 
        cataFoldX (fun x _ -> x)
                  (fun _ -> ( ** ))
                  (fun _ -> ( + ))
                  (fun _ -> ( - ))
                  (fun _ -> ( * )) 
                  (fun x -> function | Parameter(grpIdx,idx) when (t(0,0)) = (Parameter(0,0)) -> Input(Parameter(grpIdx,idx+i))
                                     | Variable(grpIdx,idx) when (t(0,0)) = (Variable(0,0)) -> Input(Variable(grpIdx,idx+i)) 
                                     | Innovation(grpIdx,idx) when (t(0,0)) = (Innovation(0,0)) -> Input(Innovation(grpIdx,idx+i))
                                     | _ -> x)  
                  g
        
    // Create a State Monad from the Graph
    // Working with monads makes composing computations easier and,
    // allows one to implement abstract ideas easily without caring to much about the details.
    // However, for speedy computations Monads may not be the best option.
    // In the far end, one should create a compiler with faster than light compiled Monad operations.
    // But for now, I trust 'FSharpPlus' to efficiently compile my Monads.
    // [TESTED]
    let inline toMonad (g:Graph<'T>) = 
        let rec loop g : ContT<State<S<'T>,'b>,'T> = monad {
            match g with
            | Constant(value) -> return value
            | Polynomial(_,0) -> return LanguagePrimitives.GenericOne
            | Polynomial(g,1) -> return! loop g
            | Polynomial(g,pwr) -> return! ( * ) <!> (loop g) <*> (loop (Polynomial(g,pwr-1)))
            | Addition(l,r) -> return! ( + ) <!> (loop l) <*> (loop r)
            | Substraction(l,r) -> return! ( - ) <!> (loop l) <*> (loop r)
            | Multiplication(l,r) -> return! ( * ) <!> (loop l) <*> (loop r)
            | Input(i) -> match i with
                              | Parameter(grpidx,idx) -> return! lift (GraphState.parameterM grpidx idx)
                              | Variable(grpidx,idx) -> return! lift (GraphState.variableM grpidx idx)
                              | Innovation(grpidx,idx) -> return! lift (GraphState.innovationM grpidx idx)
        }
        loop >> ContT.eval <| g

    // Get the default 'S' for a given graph.
    // This function makes sure to not take 'BasicInput's with same indices twice.
    // [COSTLY]
    // [TESTED]
    let defaultState (g:Graph<'T>) = 
        collectInputs
          // Make sure at least 1 BasicInput is present (group and indiv. idx should never be negative)
          >> flip Array.append [|Parameter(-1,-1);Variable(-1,-1);Innovation(-1,-1)|] 
          // Group same BasicInput (regardless of grp)
          >> Array.groupBy (function | Parameter(_,_) -> 0 | Variable(_,_) -> 1 | Innovation(_,_) -> 2) 
          // Sort array of 'Parameters' then 'Variable' then 'Innovation'
          >> Array.sortBy fst   
          >> Array.map (snd // Inside each BasicInput array, create subgroup
                            >> Array.groupBy(function | Parameter(grp,_) -> grp 
                                                      | Variable(grp,_) -> grp 
                                                      | Innovation(grp,_) -> grp) 
                            // Sort by group index (to have grp '-1' first) and get rid of the grp '-1'
                            >> Array.sortBy fst >> Array.tail
                            // make sure we don't take same BasicInput twice and replace each by an array of zeros
                            >> Array.map (snd >> Array.distinct >> Array.length >> Array.zeroCreate) 
                            // convert to 'Array2D'
                            >> array2D)
          >> (fun a -> S(a.[0],a.[1],a.[2])) <| g
                        
          
    let inline (|Constant0|Constant1|Constant_1|Other|) (g:Graph<'T>) = 
        match g with
        | Constant((c:'T)) when c = LanguagePrimitives.GenericZero -> Constant0
        | Constant((c:'T)) when c = LanguagePrimitives.GenericOne -> Constant1
        | Constant((c:'T)) when c = -LanguagePrimitives.GenericOne -> Constant_1
        | _ -> Other

    // Not tail-recursive !
    let inline simplify (g:Graph<'T>) = 
                  // Constant
        cataFoldX (fun x _ -> x)  
                  // Polynomial
                  (fun _ g e -> match g,e with
                                | _,0 -> Constant(LanguagePrimitives.GenericOne)
                                | _,1 -> g
                                | Constant0,_ -> Constant(LanguagePrimitives.GenericZero)
                                | Constant1,_ -> Constant(LanguagePrimitives.GenericOne)
                                | Polynomial(g,e2),e1 -> Polynomial(g,e1*e2)
                                | _ -> g ** e)
                  // Addition
                  (fun _ l r -> match l,r with
                                | Constant0,_ -> r 
                                | _,Constant0 -> l
                                | _ -> l + r)
                  // Substraction
                  (fun _ l r -> match l,r with
                                | Constant0,_ -> Constant(-LanguagePrimitives.GenericOne) * r
                                | _,Constant0 -> l
                                | _ -> l - r)
                  // Multiplication
                  (fun _ l r -> match l,r with
                                | Constant0,_ -> Constant(LanguagePrimitives.GenericZero)
                                | _,Constant0 -> Constant(LanguagePrimitives.GenericZero)
                                | Constant1,_ -> r 
                                | _,Constant1 -> l
                                | Constant_1,Constant(x) -> Constant(-x)
                                | Constant(x),Constant_1 -> Constant(-x)
                                | _ -> l * r)
                  // Inputs
                  (fun x _ -> x)
                  g

    let inline convertInt (n:int) = Seq.init n (fun _ -> LanguagePrimitives.GenericOne) |> Seq.sum

    // Create a graph representing the gradient of a given graph for a given parameter.
    // Fst : current gradient
    // Snd : current value
    let inline gradientForParameter grpIdx idx (g:Graph<'T>) = 
                  // Constant
        cataFoldX (fun x _ -> Constant(LanguagePrimitives.GenericZero),x)
                  // Polynomial
                  (fun x g e -> fst g * Constant(convertInt e) * Polynomial(snd g,e-1) |> simplify ,x)
                  // Addition
                  (fun x l r -> fst l + fst r |> simplify, x)
                  // Substraction
                  (fun x l r -> fst l - fst r |> simplify, x)
                  // Multiplication
                  (fun x l r -> (fst l * snd r) + (fst r * snd l) |> simplify,x)
                  // Inputs
                  (fun x -> function | Parameter(grp,i) -> if (grp=grpIdx && idx=i) then Constant(LanguagePrimitives.GenericOne),x else Constant(LanguagePrimitives.GenericZero),x
                                     | Variable(_,_) -> Constant(LanguagePrimitives.GenericZero),x
                                     | Innovation(_,_) -> Constant(LanguagePrimitives.GenericZero),x)
                  >> fst // retrieve gradient
                  <| g |> simplify

    // Create an 'Array' of 'Graph's representing the gradient for 'Parameter's of a given group. 
    let inline gradientForGroup grpIdx (g:Graph<'T>) = 
        (countUnique >> Array.choose (function | Parameter(grp,idx) -> if grpIdx=grp then Some idx else None
                                               | _ -> None)
                     >> Array.item 0
                     >> flip Array.init id) g
                     |>  Array.map (fun i -> gradientForParameter grpIdx i g)

    let inline gradient (gs:Graph<'T>[]) = Array.mapi gradientForGroup >> array2D <| gs








        








        



        
namespace ComputationalGraph

open System.Runtime.CompilerServices
open FSharpPlus.Data
open FSharpPlus.Control
open FSharpPlus
open GraphType

module Graph =   
    
    let add g1 g2 = Addition(g1,g2)
    let multiply g1 g2 = Multiplication(g1,g2)

    let equal g1 g2 = 
        let opCont l1 r1 l2 r2 f : Cont<'a,bool> = monad { let! xl = f l1 l2
                                                           let! xr = f r1 r2
                                                           return xl && xr }
        let rec loop g1 g2 = monad {
            match g1,g2 with
            | Input(i1),Input(i2) -> return i1 = i2
            | Addition(l1,r1),Addition(l2,r2) -> return! opCont l1 r1 l2 r2 loop
            | Multiplication(l1,r1),Multiplication(l2,r2) -> return! opCont l1 r1 l2 r2 loop
            | _ -> return false         // !!!
        }
        Cont.run (loop g1 g2) id
    
    let inline cataFoldX constF addF multF inputF (x:Graph) = 
        let opCont opF x lg rg f : Cont<'a,'b> = monad { let! xl = f lg
                                                         let! xr = f rg
                                                         return opF x xl xr }
        let rec loop g = monad {
            match g with
            | Constant(value) as x -> return constF x value
            | Addition(lg,rg) as x -> return! opCont addF x lg rg loop
            | Multiplication(lg,rg) as x -> return! opCont multF x lg rg loop
            | Input(i) as x -> return inputF x i
        }
        Cont.run (loop x) id

    let inline cataFold constF addF multF inputF = 
        cataFoldX (fun _ -> constF) (fun _ -> addF) (fun _ -> multF) (fun _ -> inputF)

    // Collect in an array all the subgraphs from the graph 'x'
    let collectSubGraphs (x:Graph) = 
        cataFoldX (fun x _ acc -> x::acc)
                  (fun x lacc racc acc -> x::(racc >> lacc) acc)
                  (fun x lacc racc acc -> x::(racc >> lacc) acc)
                  (fun x _ acc -> x::acc)
                  x [] |> Array.ofList

    // Concatenate in an array all the 'Input' nodes of the graph 'x'. 
    let collectInputs (x:Graph) = 
        cataFold (fun _ acc -> acc)
                 (fun lacc racc acc -> lacc (racc acc))
                 (fun lacc racc acc -> lacc (racc acc))
                 (fun i acc -> i::acc)
                 x [] |> Array.ofList

    // Get the 'Input's contained in the graph as an array.
    // There is only one 'Input' type -e.g. Parameter(_,_), Variable(_,_)- for each group of 'Input'.
    // For instance if the output array is of the form : [|Parameter(0,2);Variable(1,3)|], 
    // It translates into a graph with 2 'Parameter's in group 0,
    // and 3 'Variable's in group 1.
    let groupSizes = 
        collectInputs >> Array.countBy (function | Parameter(grpidx,_) -> Parameter(grpidx,0)
                                                 | Variable(grpidx,_) -> Variable(grpidx,0)
                                                 | Innovation(grpidx,_) -> Innovation(grpidx,0))
                      >> Array.map (function | Parameter(grpidx,_),c -> Parameter(grpidx,c)
                                             | Variable(grpidx,_),c -> Variable(grpidx,c)
                                             | Innovation(grpidx,_),c -> Innovation(grpidx,c))
                      >> Array.sortBy (function | Parameter(grpidx,_) | Variable(grpidx,_) | Innovation(grpidx,_) -> grpidx)

    // Change the 'Group Index' of some 'BasicInput' from 'oldGrp' to 'newGrp'.
    let changeGroup (oldGrp:int) (newGrp:int) =
        cataFold (fun v -> Constant(v))
                 (fun l r -> Addition(l,r))
                 (fun l r -> Multiplication(l,r))
                 (function | Parameter(grpidx,idx) as x -> if grpidx=oldGrp then Input(Parameter(newGrp,idx)) else Input(x)
                           | Variable(grpidx,idx) as x -> if grpidx=oldGrp then Input(Variable(newGrp,idx)) else Input(x)
                           | Innovation(grpidx,idx) as x -> if grpidx=oldGrp then Input(Innovation(newGrp,idx)) else Input(x))
    
    // Run the graph with the indices of the Inputs as data.
    // This method was primarily to test the graph's computations.
    let run = cataFold id (+) (*) (function Parameter(_,idx) | Variable(_,idx) | Innovation(_,idx) -> float32 idx)
        
    // Create a State Monad from the Graph
    // Working with monads makes composing computations easier and,
    // allows one to implement abstract ideas easily without caring to much about the details.
    // However, for speedy computations Monads may not be the best option.
    // In the far end, one should create a compiler with faster than light compiled Monad operations.
    // But for now, I trust 'FSharpPlus' to efficiently compile my Monads.
    let toMonad = 
        let rec loop g : ContT<State<S,'b>,float32> = monad {
            match g with
            | Constant(value) -> return! lift (result value)
            | Addition(l,r) -> return! (+) <!> (loop l) <*> (loop r)
            | Multiplication(l,r) -> return! (*) <!> (loop l) <*> (loop r)
            | Input(i) -> match i with
                              | Parameter(grpidx,idx) -> return! lift (StateGraph.parameterM grpidx idx)
                              | Variable(grpidx,idx) -> return! lift (StateGraph.variableM grpidx idx)
                              | Innovation(grpidx,idx) -> return! lift (StateGraph.innovationM idx)
        }
        loop >> ContT.eval  

    let defaultState : (Graph -> BasicInput[,][]) = 
        collectInputs
          >> Array.groupBy (function | Parameter(_,_) -> 0 | Variable(_,_) -> 1 | Innovation(_,_) -> 2)
          >> Array.sortBy fst
          >> Array.map (snd >> Array.groupBy(function | Parameter(grp,_) -> grp 
                                                      | Variable(grp,_) -> grp 
                                                      | Innovation(grp,_) -> grp) 
                            >> Array.map (snd >> Array.distinct >> Array.length >> Array.zeroCreate)
                            >> Array2D.ofArray)



    // The most important part of this project.
    // Creating a computational graph eases creation of complex computations. 
    // The ideal would be to begin with a classical computational graph and then convert/compile it to blazingly fast arrays operations. 
(*    type Graph = 
        | Input of BasicInput
        | Constant of value:float32
        | Addition of Graph * Graph
        | Multiplication of Graph * Graph
        static member inline get_Zero() = Constant(LanguagePrimitives.GenericZero)
        static member inline ( + ) (l:Graph, r:Graph) = Addition(l,r)
        static member inline ( * ) (l:Graph, r:Graph) = Multiplication(l,r)
        static member inline op_Equality (g1:Graph, g2:Graph) = 
            let opCont l1 r1 l2 r2 f : Cont<'a,bool> = monad { let! xl = f l1 l2
                                                               let! xr = f r1 r2
                                                               return xl && xr }
            let rec loop g1 g2 = monad {
                match g1,g2 with
                | Input(i1),Input(i2) -> return i1 = i2
                | Addition(l1,r1),Addition(l2,r2) -> return! opCont l1 r1 l2 r2 loop
                | Multiplication(l1,r1),Multiplication(l2,r2) -> return! opCont l1 r1 l2 r2 loop
                | _ -> return false         // !!!
            }
            Cont.run (loop g1 g2) id*)




        








        



        
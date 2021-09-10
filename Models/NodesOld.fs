
module test1

    let x = 0
(*namespace Models

module InputCounter =
    type T = T of (Input * int) array

    let empty = T [|(Innovation,0);(Parameter,0);(Variable,0);(PreviousResult,0);(Constant,0)|]

    let indexOf = function
        | Innovation -> 0
        | Parameter -> 1
        | Variable -> 2
        | PreviousResult -> 3
        | Constant -> 4

    let toArray (T(counter)) = counter

    let fromArray array = 
        let (T(tmp)) = empty
        tmp |> Array.map (fun xtmp -> if Array.exists (fun x -> (fst x) = (fst xtmp)) array then 
                                        (fst xtmp), (snd xtmp) + snd (array |> Array.filter (fun x -> (fst x) = (fst xtmp))).[0]
                                        else xtmp)
            |> T

    let map f (T(counter)) = counter |> Array.map f |> T

    let AddOneIfZero counter = map (fun x -> if (snd x) = 0 then (fst x),1 else x) counter

    let Add (T(counter1)) (T(counter2)) = 
        counter2 |> Array.map2 (fun x1 x2 -> (fst x1, (snd x1) + (snd x2))) counter1 
                 |> T

    let AmountOf input (T(counter)) = 
        counter.[indexOf input] |> snd

module Node = 
    let inline ( +. ) (N1:Skeleton) (N2:Skeleton) = Node(Addition, N1, N2)
    let inline ( *. ) (N1:Skeleton) (N2:Skeleton) = Node(Multiplication, N1, N2)
    let inline ( -. ) (N1:Skeleton) (N2:Skeleton) = Node(Substraction, N1, N2)
    let inline ( <. ) (N1:Skeleton) (N2:Skeleton) = Node(LessThan, N1, N2)

    let shiftSkeletonBy shift sk = 
        Graph.Skeleton.fold (fun op kl kr n k -> kl (fun lacc -> kr (fun racc -> Node(op,lacc,racc) |> k)))
                           (fun input x idx pullFrom n k -> let nb = InputCounter.AmountOf input shift
                                                            let shiftedStrat = Option.map (fun x -> fst x, (snd x) + (InputCounter.AmountOf (fst x) shift)) pullFrom
                                                            Leaf(input,x,idx+nb,shiftedStrat) |> k)
                           sk

    // Create a Node that mixes linearly the given skeleton (a node in the graph)
    // The node is usefull for creating Mixture models (Thresholds, Markov-Chains,Smooth Transitions,...)
    // Parameters : mixingNode -> Node that should output between [0;1].
    //              sk -> Skeleton to be duplicated
    let mixture mixingNode sk1 sk2 = 
        let inputNodeSk1 = Graph.Skeleton.countNodeByInput sk1 |> InputCounter.fromArray
        let inputNodeSk2 = Graph.Skeleton.countNodeByInput sk2 |> InputCounter.fromArray

        let shiftedSk2 = shiftSkeletonBy inputNodeSk1 sk2

        let totalCount = InputCounter.Add inputNodeSk1 inputNodeSk2
        let shiftedMixingNode = shiftSkeletonBy totalCount mixingNode
        

        (shiftedMixingNode *. sk1) +. ((Leaf(Constant, Some 1.0,0,None) -. shiftedMixingNode) *. shiftedSk2)*)


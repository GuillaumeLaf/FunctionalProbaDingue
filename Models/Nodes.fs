namespace Models

module Node = 
    let inline ( +. ) (N1:Skeleton) (N2:Skeleton) = Node(Addition, N1, N2)
    let inline ( *. ) (N1:Skeleton) (N2:Skeleton) = Node(Multiplication, N1, N2)
    let inline ( -. ) (N1:Skeleton) (N2:Skeleton) = Node(Substraction, N1, N2)
    let inline ( <. ) (N1:Skeleton) (N2:Skeleton) = Node(LessThan, N1, N2)

    // Create a Node that duplicates (!) and mixes linearly the given skeleton (a node in the graph)
    // The node is usefull for creating Mixture models (Thresholds, Markov-Chains,Smooth Transitions,...)
    // Parameters : mixingNode -> Node that should output between [0;1].
    //              sk -> Skeleton to be duplicated
(*    let mixture mixingNode sk1 sk2 = 
        
        (mixingNode *. sk1) +. *)

